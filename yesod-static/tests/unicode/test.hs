import Network.Wai.Application.Static

main = do
    checkPieces "." ["test.hs"] >>= print
    checkPieces "." ["test.hs", ""] >>= print
    checkPieces "." ["Network", ""] >>= print
    checkPieces "." ["Network"] >>= print
