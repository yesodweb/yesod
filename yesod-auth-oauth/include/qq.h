
-- CPP macro which choses which quasyquotes syntax to use depending
-- on GHC version.
--
-- QQ stands for quasyquote.
#if GHC7
# define QQ(x) x
#else
# define QQ(x) $x
#endif
