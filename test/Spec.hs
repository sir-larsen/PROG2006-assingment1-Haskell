import Test.DocTest(doctest)

main :: IO ()
main = do
    doctest ["-isrc", "app/Main.hs"] -- Running the doctests
