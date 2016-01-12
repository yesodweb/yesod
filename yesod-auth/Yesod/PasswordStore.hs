{-# LANGUAGE OverloadedStrings, BangPatterns #-}
{-# LANGUAGE CPP #-}
-- |
-- Module      : Crypto.PasswordStore
-- Copyright   : (c) Peter Scott, 2011
-- License     : BSD-style
--
-- Maintainer  : pjscott@iastate.edu
-- Stability   : experimental
-- Portability : portable
--
-- Securely store hashed, salted passwords. If you need to store and verify
-- passwords, there are many wrong ways to do it, most of them all too
-- common. Some people store users' passwords in plain text. Then, when an
-- attacker manages to get their hands on this file, they have the passwords for
-- every user's account. One step up, but still wrong, is to simply hash all
-- passwords with SHA1 or something. This is vulnerable to rainbow table and
-- dictionary attacks. One step up from that is to hash the password along with
-- a unique salt value. This is vulnerable to dictionary attacks, since guessing
-- a password is very fast. The right thing to do is to use a slow hash
-- function, to add some small but significant delay, that will be negligible
-- for legitimate users but prohibitively expensive for someone trying to guess
-- passwords by brute force. That is what this library does. It iterates a
-- SHA256 hash, with a random salt, a few thousand times. This scheme is known
-- as PBKDF1, and is generally considered secure; there is nothing innovative
-- happening here.
--
-- The API here is very simple. What you store are called /password hashes/.
-- They are strings (technically, ByteStrings) that look like this:
--
-- > "sha256|14|jEWU94phx4QzNyH94Qp4CQ==|5GEw+jxP/4WLgzt9VS3Ee3nhqBlDsrKiB+rq7JfMckU="
--
-- Each password hash shows the algorithm, the strength (more on that later),
-- the salt, and the hashed-and-salted password. You store these on your server,
-- in a database, for when you need to verify a password. You make a password
-- hash with the 'makePassword' function. Here's an example:
--
-- > >>> makePassword "hunter2" 14
-- > "sha256|14|Zo4LdZGrv/HYNAUG3q8WcA==|zKjbHZoTpuPLp1lh6ATolWGIKjhXvY4TysuKvqtNFyk="
--
-- This will hash the password @\"hunter2\"@, with strength 14, which is a good
-- default value. The strength here determines how long the hashing will
-- take. When doing the hashing, we iterate the SHA256 hash function
-- @2^strength@ times, so increasing the strength by 1 makes the hashing take
-- twice as long. When computers get faster, you can bump up the strength a
-- little bit to compensate. You can strengthen existing password hashes with
-- the 'strengthenPassword' function. Note that 'makePassword' needs to generate
-- random numbers, so its return type is 'IO' 'ByteString'. If you want to avoid
-- the 'IO' monad, you can generate your own salt and pass it to
-- 'makePasswordSalt'.
--
-- Your strength value should not be less than 12, and 14 is a good default
-- value at the time of this writing, in 2013.
--
-- Once you've got your password hashes, the second big thing you need to do
-- with them is verify passwords against them. When a user gives you a password,
-- you compare it with a password hash using the 'verifyPassword' function:
--
-- > >>> verifyPassword "wrong guess" passwordHash
-- > False
-- > >>> verifyPassword "hunter2" passwordHash
-- > True
--
-- These two functions are really all you need. If you want to make existing
-- password hashes stronger, you can use 'strengthenPassword'. Just pass it an
-- existing password hash and a new strength value, and it will return a new
-- password hash with that strength value, which will match the same password as
-- the old password hash.
--
-- Note that, as of version 2.4, you can also use PBKDF2, and specify the exact
-- iteration count. This does not have a significant effect on security, but can
-- be handy for compatibility with other code.

module Yesod.PasswordStore (

        -- * Algorithms
        pbkdf1,                 -- :: ByteString -> Salt -> Int -> ByteString
        pbkdf2,                 -- :: ByteString -> Salt -> Int -> ByteString

        -- * Registering and verifying passwords
        makePassword,           -- :: ByteString -> Int -> IO ByteString
        makePasswordWith,       -- :: (ByteString -> Salt -> Int -> ByteString) ->
                                --    ByteString -> Int -> IO ByteString
        makePasswordSalt,       -- :: ByteString -> ByteString -> Int -> ByteString
        makePasswordSaltWith,   -- :: (ByteString -> Salt -> Int -> ByteString) ->
                                --    ByteString -> Salt -> Int -> ByteString
        verifyPassword,         -- :: ByteString -> ByteString -> Bool
        verifyPasswordWith,     -- :: (ByteString -> Salt -> Int -> ByteString) ->
                                --    (Int -> Int) -> ByteString -> ByteString -> Bool

        -- * Updating password hash strength
        strengthenPassword,     -- :: ByteString -> Int -> ByteString
        passwordStrength,       -- :: ByteString -> Int

        -- * Utilities
        Salt,
        isPasswordFormatValid,  -- :: ByteString -> Bool
        genSaltIO,              -- :: IO Salt
        genSaltRandom,          -- :: (RandomGen b) => b -> (Salt, b)
        makeSalt,               -- :: ByteString -> Salt
        exportSalt,             -- :: Salt -> ByteString
        importSalt              -- :: ByteString -> Salt
  ) where


import qualified Crypto.Hash as CH
import qualified Crypto.Hash.SHA256 as H
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as Binary
import Control.Monad
import Control.Monad.ST
import Data.Byteable (toBytes)
import Data.STRef
import Data.Bits
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Base64 (encode, decodeLenient)
import System.IO
import System.Random
import Data.Maybe
import qualified Control.Exception

---------------------
-- Cryptographic base
---------------------

-- | PBKDF1 key-derivation function. Takes a password, a 'Salt', and a number of
-- iterations. The number of iterations should be at least 1000, and probably
-- more. 5000 is a reasonable number, computing almost instantaneously. This
-- will give a 32-byte 'ByteString' as output. Both the salt and this 32-byte
-- key should be stored in the password file. When a user wishes to authenticate
-- a password, just pass it and the salt to this function, and see if the output
-- matches.
pbkdf1 :: ByteString -> Salt -> Int -> ByteString
pbkdf1 password (SaltBS salt) iter = hashRounds first_hash (iter + 1)
    where first_hash = H.finalize $ H.init `H.update` password `H.update` salt

-- | Hash a 'ByteString' for a given number of rounds. The number of rounds is 0
-- or more. If the number of rounds specified is 0, the ByteString will be
-- returned unmodified.
hashRounds :: ByteString -> Int -> ByteString
hashRounds (!bs) 0 = bs
hashRounds bs rounds = hashRounds (H.hash bs) (rounds - 1)

-- | Computes the hmacSHA256 of the given message, with the given 'Salt'.
hmacSHA256 :: ByteString
           -- ^ The secret (the salt)
           -> ByteString
           -- ^ The clear-text message
           -> ByteString
           -- ^ The encoded message
hmacSHA256 secret msg =
    toBytes (CH.hmacGetDigest (CH.hmac secret msg) :: CH.Digest CH.SHA256)

-- | PBKDF2 key-derivation function.
-- For details see @http://tools.ietf.org/html/rfc2898@.
-- @32@ is the most common digest size for @SHA256@, and is
-- what the algorithm internally uses.
-- @HMAC+SHA256@ is used as @PRF@, because @HMAC+SHA1@ is considered too weak.
pbkdf2 :: ByteString -> Salt -> Int -> ByteString
pbkdf2 password (SaltBS salt) c =
    let hLen = 32
        dkLen = hLen in go hLen dkLen
  where
    go hLen dkLen | dkLen > (2^32 - 1) * hLen = error "Derived key too long."
                  | otherwise =
                      let !l = ceiling ((fromIntegral dkLen / fromIntegral hLen) :: Double)
                          !r = dkLen - (l - 1) * hLen
                          chunks = [f i | i <- [1 .. l]]
                      in (B.concat . init $ chunks) `B.append` B.take r (last chunks)

    -- The @f@ function, as defined in the spec.
    -- It calls 'u' under the hood.
    f :: Int -> ByteString
    f i = let !u1 = hmacSHA256 password (salt `B.append` int i)
      -- Using the ST Monad, for maximum performance.
      in runST $ do
          u <- newSTRef u1
          accum <- newSTRef u1
          forM_ [2 .. c] $ \_ -> do
            modifySTRef' u (hmacSHA256 password)
            currentU <- readSTRef u
            modifySTRef' accum (`xor'` currentU)
          readSTRef accum

    -- int(i), as defined in the spec.
    int :: Int -> ByteString
    int i = let str = BL.unpack . Binary.encode $ i
            in BS.pack $ drop (length str - 4) str

    -- | A convenience function to XOR two 'ByteString' together.
    xor' :: ByteString -> ByteString -> ByteString
    xor' !b1 !b2 = BS.pack $ BS.zipWith xor b1 b2

-- | Generate a 'Salt' from 128 bits of data from @\/dev\/urandom@, with the
-- system RNG as a fallback. This is the function used to generate salts by
-- 'makePassword'.
genSaltIO :: IO Salt
genSaltIO =
    Control.Exception.catch genSaltDevURandom def
  where
    def :: IOError -> IO Salt
    def _ = genSaltSysRandom

-- | Generate a 'Salt' from @\/dev\/urandom@.
genSaltDevURandom :: IO Salt
genSaltDevURandom = withFile "/dev/urandom" ReadMode $ \h -> do
                      rawSalt <- B.hGet h 16
                      return $ makeSalt rawSalt

-- | Generate a 'Salt' from 'System.Random'.
genSaltSysRandom :: IO Salt
genSaltSysRandom = randomChars >>= return . makeSalt . B.pack
    where randomChars = sequence $ replicate 16 $ randomRIO ('\NUL', '\255')

-----------------------
-- Password hash format
-----------------------

-- Format: "sha256|strength|salt|hash", where strength is an unsigned int, salt
-- is a base64-encoded 16-byte random number, and hash is a base64-encoded hash
-- value.

-- | Try to parse a password hash.
readPwHash :: ByteString -> Maybe (Int, Salt, ByteString)
readPwHash pw | length broken /= 4
                || algorithm /= "sha256"
                || B.length hash /= 44 = Nothing
              | otherwise = case B.readInt strBS of
                              Just (strength, _) -> Just (strength, SaltBS salt, hash)
                              Nothing -> Nothing
    where broken = B.split '|' pw
          [algorithm, strBS, salt, hash] = broken

-- | Encode a password hash, from a @(strength, salt, hash)@ tuple, where
-- strength is an 'Int', and both @salt@ and @hash@ are base64-encoded
-- 'ByteString's.
writePwHash :: (Int, Salt, ByteString) -> ByteString
writePwHash (strength, SaltBS salt, hash) =
    B.intercalate "|" ["sha256", B.pack (show strength), salt, hash]

-----------------
-- High level API
-----------------

-- | Hash a password with a given strength (14 is a good default). The output of
-- this function can be written directly to a password file or
-- database. Generates a salt using high-quality randomness from
-- @\/dev\/urandom@ or (if that is not available, for example on Windows)
-- 'System.Random', which is included in the hashed output.
makePassword :: ByteString -> Int -> IO ByteString
makePassword = makePasswordWith pbkdf1

-- | A generic version of 'makePassword', which allow the user
-- to choose the algorithm to use.
--
-- >>> makePasswordWith pbkdf1 "password" 14
--
makePasswordWith :: (ByteString -> Salt -> Int -> ByteString)
                 -- ^ The algorithm to use (e.g. pbkdf1)
                 -> ByteString
                 -- ^ The password to encrypt
                 -> Int
                 -- ^ log2 of the number of iterations
                 -> IO ByteString
makePasswordWith algorithm password strength = do
  salt <- genSaltIO
  return $ makePasswordSaltWith algorithm (2^) password salt strength

-- | A generic version of 'makePasswordSalt', meant to give the user
-- the maximum control over the generation parameters.
-- Note that, unlike 'makePasswordWith', this function takes the @raw@
-- number of iterations. This means the user will need to specify a
-- sensible value, typically @10000@ or @20000@.
makePasswordSaltWith :: (ByteString -> Salt -> Int -> ByteString)
                     -- ^ A function modeling an algorithm (e.g. 'pbkdf1')
                     -> (Int -> Int)
                     -- ^ A function to modify the strength
                     -> ByteString
                     -- ^ A password, given as clear text
                     -> Salt
                     -- ^ A hash 'Salt'
                     -> Int
                     -- ^ The password strength (e.g. @10000, 20000, etc.@)
                     -> ByteString
makePasswordSaltWith algorithm strengthModifier pwd salt strength = writePwHash (strength, salt, hash)
    where hash = encode $ algorithm pwd salt (strengthModifier strength)

-- | Hash a password with a given strength (14 is a good default), using a given
-- salt. The output of this function can be written directly to a password file
-- or database. Example:
--
-- > >>> makePasswordSalt "hunter2" (makeSalt "72cd18b5ebfe6e96") 14
-- > "sha256|14|NzJjZDE4YjVlYmZlNmU5Ng==|yuiNrZW3KHX+pd0sWy9NTTsy5Yopmtx4UYscItSsoxc="
makePasswordSalt :: ByteString -> Salt -> Int -> ByteString
makePasswordSalt = makePasswordSaltWith pbkdf1 (2^)

-- | 'verifyPasswordWith' @algorithm userInput pwHash@ verifies
-- the password @userInput@ given by the user against the stored password
-- hash @pwHash@, with the hashing algorithm @algorithm@.  Returns 'True' if the
-- given password is correct, and 'False' if it is not.
-- This function allows the programmer to specify the algorithm to use,
-- e.g. 'pbkdf1' or 'pbkdf2'.
-- Note: If you want to verify a password previously generated with
-- 'makePasswordSaltWith', but without modifying the number of iterations,
-- you can do:
--
-- > >>> verifyPasswordWith pbkdf2 id "hunter2" "sha256..."
-- > True
--
verifyPasswordWith :: (ByteString -> Salt -> Int -> ByteString)
                   -- ^ A function modeling an algorithm (e.g. pbkdf1)
                   -> (Int -> Int)
                   -- ^ A function to modify the strength
                   -> ByteString
                   -- ^ User password
                   -> ByteString
                   -- ^ The generated hash (e.g. sha256|14...)
                   -> Bool
verifyPasswordWith algorithm strengthModifier userInput pwHash =
    case readPwHash pwHash of
      Nothing -> False
      Just (strength, salt, goodHash) ->
          encode (algorithm userInput salt (strengthModifier strength)) == goodHash

-- | Like 'verifyPasswordWith', but uses 'pbkdf1' as algorithm.
verifyPassword :: ByteString -> ByteString -> Bool
verifyPassword = verifyPasswordWith pbkdf1 (2^)

-- | Try to strengthen a password hash, by hashing it some more
-- times. @'strengthenPassword' pwHash new_strength@ will return a new password
-- hash with strength at least @new_strength@. If the password hash already has
-- strength greater than or equal to @new_strength@, then it is returned
-- unmodified. If the password hash is invalid and does not parse, it will be
-- returned without comment.
--
-- This function can be used to periodically update your password database when
-- computers get faster, in order to keep up with Moore's law. This isn't hugely
-- important, but it's a good idea.
strengthenPassword :: ByteString -> Int -> ByteString
strengthenPassword pwHash newstr =
    case readPwHash pwHash of
      Nothing -> pwHash
      Just (oldstr, salt, hashB64) ->
          if oldstr < newstr then
              writePwHash (newstr, salt, newHash)
          else
              pwHash
          where newHash = encode $ hashRounds hash extraRounds
                extraRounds = (2^newstr) - (2^oldstr)
                hash = decodeLenient hashB64

-- | Return the strength of a password hash.
passwordStrength :: ByteString -> Int
passwordStrength pwHash = case readPwHash pwHash of
                            Nothing               -> 0
                            Just (strength, _, _) -> strength

------------
-- Utilities
------------

-- | A salt is a unique random value which is stored as part of the password
-- hash. You can generate a salt with 'genSaltIO' or 'genSaltRandom', or if you
-- really know what you're doing, you can create them from your own ByteString
-- values with 'makeSalt'.
newtype Salt = SaltBS ByteString
    deriving (Show, Eq, Ord)

-- | Create a 'Salt' from a 'ByteString'. The input must be at least 8
-- characters, and can contain arbitrary bytes. Most users will not need to use
-- this function.
makeSalt :: ByteString -> Salt
makeSalt = SaltBS . encode . check_length
    where check_length salt | B.length salt < 8 =
                                error "Salt too short. Minimum length is 8 characters."
                            | otherwise = salt

-- | Convert a 'Salt' into a 'ByteString'. The resulting 'ByteString' will be
-- base64-encoded. Most users will not need to use this function.
exportSalt :: Salt -> ByteString
exportSalt (SaltBS bs) = bs

-- | Convert a raw 'ByteString' into a 'Salt'.
-- Use this function with caution, since using a weak salt will result in a
-- weak password.
importSalt :: ByteString -> Salt
importSalt = SaltBS

-- | Is the format of a password hash valid? Attempts to parse a given password
-- hash. Returns 'True' if it parses correctly, and 'False' otherwise.
isPasswordFormatValid :: ByteString -> Bool
isPasswordFormatValid = isJust . readPwHash

-- | Generate a 'Salt' with 128 bits of data taken from a given random number
-- generator. Returns the salt and the updated random number generator. This is
-- meant to be used with 'makePasswordSalt' by people who would prefer to either
-- use their own random number generator or avoid the 'IO' monad.
genSaltRandom :: (RandomGen b) => b -> (Salt, b)
genSaltRandom gen = (salt, newgen)
    where rands _ 0 = []
          rands g n = (a, g') : rands g' (n-1 :: Int)
              where (a, g') = randomR ('\NUL', '\255') g
          salt   = makeSalt $ B.pack $ map fst (rands gen 16)
          newgen = snd $ last (rands gen 16)

#if !MIN_VERSION_base(4, 6, 0)
-- | Strict version of 'modifySTRef'
modifySTRef' :: STRef s a -> (a -> a) -> ST s ()
modifySTRef' ref f = do
    x <- readSTRef ref
    let x' = f x
    x' `seq` writeSTRef ref x'
#endif

#if MIN_VERSION_bytestring(0, 10, 0)
toStrict :: BL.ByteString -> BS.ByteString
toStrict = BL.toStrict

fromStrict :: BS.ByteString -> BL.ByteString
fromStrict = BL.fromStrict
#else
toStrict :: BL.ByteString -> BS.ByteString
toStrict = BS.concat . BL.toChunks

fromStrict :: BS.ByteString -> BL.ByteString
fromStrict = BL.fromChunks . return
#endif
