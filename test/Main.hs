module Main (main) where

import Sudoku
import Test.HUnit

main :: IO ()
main = do
    putStrLn "Running easy tests:"
    _ <- runTestTT testsEasy
    putStrLn "Running medium tests:"
    _ <- runTestTT testsMedium
    putStrLn "Running hard tests:"
    _ <- runTestTT testsHard
    putStrLn "Running invalidity tests:"
    _ <- runTestTT testsInvalid
    putStrLn "Running file handling tests:"
    _ <- runTestTT testsFile
    return ()

{-
EASY SUDOKUS
-}

testEasy1 :: Test
testEasy1 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "007020850200516000400000006070648090930102068060953020700000005000495002029060100") )
testEasy2 :: Test
testEasy2 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "000803000060000020005000100006459800002000700050302010830000075000106000040000080") )
testEasy3 :: Test
testEasy3 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "500004003000071600031600400480020300010807040006050081004002730002730000300500009") )
testEasy4 :: Test
testEasy4 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "403000059700400600016900708000807320000020000052304000108006930004003001360000407") )
testEasy5 :: Test
testEasy5 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "008000070570200940030089005002000090001070500050000100600430080027008013080000200") )
testEasy6 :: Test
testEasy6 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "849000057760000028000009003002106000000030000000807400100400000690000014420000786") )
testEasy7 :: Test
testEasy7 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "000280300059047680840500090020000807930050046406000010090003065063410920001062000") )
testEasy8 :: Test
testEasy8 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "203100000506300001000700029000570200100604003002039000350007000600003102000006805") )
testEasy9 :: Test
testEasy9 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "002080000030900040090000003820500000140000092000008014700000030010004070000070900") )
testEasy10 :: Test
testEasy10 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "900000004000000000000174000000702000008060900530000086340000052007625300000309000") )
testEasy11 :: Test
testEasy11 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "300200578400039000100005000053020007040508060900040820000600001000450006671002004") )
testEasy12 :: Test
testEasy12 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "400650801080010040607400300000145903250307064301296000003004206020060030104032007") )
testEasy13 :: Test
testEasy13 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "000092000090147350047000100760405030430070065080306049006000910074938020000560000") )
testEasy14 :: Test
testEasy14 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "000000000070000835802070901094001000060807090000500310503020607618000050000000000") )
testEasy15 :: Test
testEasy15 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "000000060301596000006130980090607510065040730047301090012063800000215309030000000") )
testEasy16 :: Test
testEasy16 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "105000620000800700030100000000080170600090002091030000000009080004008000067000203") )
testEasy17 :: Test
testEasy17 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "200819034190650080000000000800030095520901073730080001000000000040097068910346007") )
testEasy18 :: Test
testEasy18 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "000249050200000700043570800500307401701060309604908005007054120002000006050182000") )
testEasy19 :: Test
testEasy19 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "600000004009000200801000705700040003050803090308902406000000000020705040900020008") )
testEasy20 :: Test
testEasy20 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "100002006092006180080000020430010000000908000000020095060000040028400760500800003") )
testEasy21 :: Test
testEasy21 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "000305000601040307400000009008000500030000020000206000000901000080030070200050001") )
testEasy22 :: Test
testEasy22 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "300000000000005940069001058080030000203408506000020070130700460052600000000000001") )
testEasy23 :: Test
testEasy23 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "005007000000009023800002006057003060000000000030100270100700008970300000000800300") )
testEasy24 :: Test
testEasy24 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "100407002200000006070000010540000021306102504001070900000301000000050000010609080") )
testEasy25 :: Test
testEasy25 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "600000005570000083000807000006409800040000010000060000030020070005080400800534006") )

testsEasy :: Test
testsEasy = TestList [ TestLabel "Easy1" testEasy1, TestLabel "Easy2" testEasy2, TestLabel "Easy3" testEasy3, TestLabel "Easy4" testEasy4, TestLabel "Easy5" testEasy5, TestLabel "Easy6" testEasy6, TestLabel "Easy7" testEasy7, TestLabel "Easy8" testEasy8, TestLabel "Easy9" testEasy9, TestLabel "Easy10" testEasy10, TestLabel "Easy11" testEasy11, TestLabel "Easy12" testEasy12, TestLabel "Easy13" testEasy13, TestLabel "Easy14" testEasy14, TestLabel "Easy15" testEasy15, TestLabel "Easy16" testEasy16, TestLabel "Easy17" testEasy17, TestLabel "Easy18" testEasy18, TestLabel "Easy19" testEasy19, TestLabel "Easy20" testEasy20, TestLabel "Easy21" testEasy21, TestLabel "Easy22" testEasy22, TestLabel "Easy23" testEasy23, TestLabel "Easy24" testEasy24, TestLabel "Easy25" testEasy25 ]

{-
MEDIUM SUDOKUS
-}

testMedium1 :: Test
testMedium1 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "020900000048000031000063020009407003003080200400105600030570000250000180000006050") )
testMedium2 :: Test
testMedium2 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "100800570000009210090040000300900050007000300020006008000020040071400000064007003") )
testMedium3 :: Test
testMedium3 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "002000800005020100460000029130060052009080400000302000006070200700000008020519070") )
testMedium4 :: Test
testMedium4 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "802600009000058000006000401090406005020000040600203090205000900000970000100002804") )
testMedium5 :: Test
testMedium5 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "070000120100000067000200004200040070710030049090070001300009000950000006067000080") )
testMedium6 :: Test
testMedium6 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "054608003700004000800000020690000102000010000203000047070000006000500008900306410") )
testMedium7 :: Test
testMedium7 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "000159000015000790000000000100405008280000067500728001000896000098010420000000000") )
testMedium8 :: Test
testMedium8 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "000000000340000091701060408800000006010000020600205009060107050005020100030090060") )
testMedium9 :: Test
testMedium9 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "206008309001002000700004012942060000000407000000080423620700004000200500309800206") )
testMedium10 :: Test
testMedium10 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "100009570798040000600002000012000008000000000500000320000300005000070416061200003") )
testMedium11 :: Test
testMedium11 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "009610720001050004000000010000400109000185000507002000080000000700090200045027300") )
testMedium12 :: Test
testMedium12 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "002806100000090000300000007003000200600704008820000045000010000140080063030050080") )
testMedium13 :: Test
testMedium13 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "010609070020701050050000090200000005000040000604805302060000080009010600000502000") )
testMedium14 :: Test
testMedium14 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "070090050500010006004000800001904300003000600960000085000000000006807100040000030") )
testMedium15 :: Test
testMedium15 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "038000006290010000540800100000100000020060040000004000004006051000090073300000290") )
testMedium16 :: Test
testMedium16 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "004060900860000014003000700000000000950642071600080002000000000025000680480010059") )
testMedium17 :: Test
testMedium17 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "450000027000050000006040800014709560090000080005020700100203008023000190000090000") )
testMedium18 :: Test
testMedium18 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "060070080107000503050104090001000800600305009034206710000020000000907000000601000") )
testMedium19 :: Test
testMedium19 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "030000000002379004004008100000080097006000500520010000008700400100834700000000080") )
testMedium20 :: Test
testMedium20 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "800000905039080620720004080002090000070406030000030100090800062048070510607000004") )
testMedium21 :: Test
testMedium21 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "300006089690800070001200300400000910000020000035000002003008500040003091950700004") )
testMedium22 :: Test
testMedium22 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "010006800060000000324000067000048090000709000030210000540000376000000040002600050") )
testMedium23 :: Test
testMedium23 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "087000910200000005000809000000030000305060802900000006800010007060000020002674500") )
testMedium24 :: Test
testMedium24 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "008509600760000094000000000000040000090802030800000002070396020009020400030000050") )
testMedium25 :: Test
testMedium25 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "003040900000207000010000050029403570580102093001000800005020600000000000060501080") )

testsMedium :: Test
testsMedium = TestList [ TestLabel "Medium1" testMedium1, TestLabel "Medium2" testMedium2, TestLabel "Medium3" testMedium3, TestLabel "Medium4" testMedium4, TestLabel "Medium5" testMedium5, TestLabel "Medium6" testMedium6, TestLabel "Medium7" testMedium7, TestLabel "Medium8" testMedium8, TestLabel "Medium9" testMedium9, TestLabel "Medium10" testMedium10, TestLabel "Medium11" testMedium11, TestLabel "Medium12" testMedium12, TestLabel "Medium13" testMedium13, TestLabel "Medium14" testMedium14, TestLabel "Medium15" testMedium15, TestLabel "Medium16" testMedium16, TestLabel "Medium17" testMedium17, TestLabel "Medium18" testMedium18, TestLabel "Medium19" testMedium19, TestLabel "Medium20" testMedium20, TestLabel "Medium21" testMedium21, TestLabel "Medium22" testMedium22, TestLabel "Medium23" testMedium23, TestLabel "Medium24" testMedium24, TestLabel "Medium25" testMedium25 ]

{-
HARD SUDOKUS
-}

testHard1 :: Test
testHard1 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "080200400570000100002300000820090005000715000700020041000006700003000018007009050") )
testHard2 :: Test
testHard2 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "600050007030000000080409200015300000008000300000007590009501030000000080200070004") )
testHard3 :: Test
testHard3 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "210950004090060037000700000000000308920000015805000000000002000680010040100047096") )
testHard4 :: Test
testHard4 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "024000650100000007008010900000000000260090083080501070600903008002854700000070000") )
testHard5 :: Test
testHard5 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "000050000000206000064000390045000810000020000000107000053000980090804060100030004") )
testHard6 :: Test
testHard6 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "108500406000070900530004007001060008090408070800050600700100069006080000904006205") )
testHard7 :: Test
testHard7 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "970306042805000109000050000207000304010020080400738001000905000000000000100847003") )
testHard8 :: Test
testHard8 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "040000000086100034001500260000305840000040000058902000095008300160009450000000010") )
testHard9 :: Test
testHard9 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "045900000000710205020003009008301026010000050360805100200100030801057000000009510") )
testHard10 :: Test
testHard10 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "900801005000607000870000069490000057080000020000375000040000070008060900109000603") )
testHard11 :: Test
testHard11 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "000006007007050000054090100090304080003060700010907050006080410000070900900100000") )
testHard12 :: Test
testHard12 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "000000000003702800060354090089000160070645030000000000040000070200506004000010000") )
testHard13 :: Test
testHard13 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "060050030000306000007000400030000060014020790700000001000000000900147005051609870") )
testHard14 :: Test
testHard14 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "103600400590001008000200000809020000207080905000070204000002000600800039005003802") )
testHard15 :: Test
testHard15 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "500000001020600700780005000904001008000908000200500904000300017009006050600000002") )
testHard16 :: Test
testHard16 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "300005007010030590020008000708000000090000010000000902000900040032080070400600001") )
testHard17 :: Test
testHard17 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "600040001030008700009700000003096000906000103000120500000002400002400080400010002") )
testHard18 :: Test
testHard18 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "200010007000207000050000020005020400001549200300708001070804030000000000630000085") )
testHard19 :: Test
testHard19 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "200040003600239008000000000029000530850000016006102900070805060900703001000010000") )
testHard20 :: Test
testHard20 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "869000312020000080070108040030000090700060004001902500000836000400070003000010000") )
testHard21 :: Test
testHard21 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "780000021960050084000000000300609008097080130100030006030806010006201900000000000") )
testHard22 :: Test
testHard22 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "024009030000600974000080020001040000207050809000060700070090000539006000080100290") )
testHard23 :: Test
testHard23 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "600589002030402060000070000000000000102060904006050800800040001010000070750020098") )
testHard24 :: Test
testHard24 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "030000200001970603540002080002000090010090030060000800070800029205039100006000050") )
testHard25 :: Test
testHard25 = TestCase (assertEqual "Solved sudoku must be valid" True (checkSudoku $ solveGridFromString "030000080000791000005000700004080500001204300200305008002508600000000000503010809") )

testsHard :: Test
testsHard = TestList [ TestLabel "Hard1" testHard1, TestLabel "Hard2" testHard2, TestLabel "Hard3" testHard3, TestLabel "Hard4" testHard4, TestLabel "Hard5" testHard5, TestLabel "Hard6" testHard6, TestLabel "Hard7" testHard7, TestLabel "Hard8" testHard8, TestLabel "Hard9" testHard9, TestLabel "Hard10" testHard10, TestLabel "Hard11" testHard11, TestLabel "Hard12" testHard12, TestLabel "Hard13" testHard13, TestLabel "Hard14" testHard14, TestLabel "Hard15" testHard15, TestLabel "Hard16" testHard16, TestLabel "Hard17" testHard17, TestLabel "Hard18" testHard18, TestLabel "Hard19" testHard19, TestLabel "Hard20" testHard20, TestLabel "Hard21" testHard21, TestLabel "Hard22" testHard22, TestLabel "Hard23" testHard23, TestLabel "Hard24" testHard24, TestLabel "Hard25" testHard25 ]

{-
INVALID GRIDS
-}

testInvalid1 :: Test
testInvalid1 = TestCase (assertEqual "Invalid sudoku input must return false (length too short)" False (checkSudoku $ solveGridFromString "") )
testInvalid2 :: Test
testInvalid2 = TestCase (assertEqual "Invalid sudoku input must return false (length too long)" False (checkSudoku $ solveGridFromString "030000080000791000005000700004080500001204300200305008002508600000000000503010809030000080000791000005000700004080500001204300200305008002508600000000000503010809") )
testInvalid3 :: Test
testInvalid3 = TestCase (assertEqual "Invalid sudoku input must return false (illegal characters)" False (checkSudoku $ solveGridFromString "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") )

testsInvalid :: Test
testsInvalid = TestList [ TestLabel "Invalid1" testInvalid1, TestLabel "Invalid2" testInvalid2, TestLabel "Invalid3" testInvalid3 ]

{-
FILE HANDLING TESTS
-}

-- a function, which given an input file path, checks whether the sudoku stored in that file has been processed and solved correctly
fileChecker :: FilePath -> IO Bool
fileChecker fp = do
  fileContents <- readGridFromFile (replaceFilePath fp)
  return (checkSudoku fileContents)

testFile1 :: Test
testFile1 = TestCase $ do
  let fp = "test/input1.txt"
  processFile fp
  result <- fileChecker fp
  assertEqual "File should contain valid sudoku" True result
testFile2 :: Test
testFile2 = TestCase $ do
  let fp = "test/input2.txt"
  processFile fp
  result <- fileChecker fp
  assertEqual "File should not contain valid sudoku (illegal input)" False result
testFile3 :: Test
testFile3 = TestCase $ do
  let fp = "test/input3.txt"
  processFile fp
  result <- fileChecker fp
  assertEqual "File should not contain valid sudoku (input too long)" False result

testsFile :: Test
testsFile = TestList [ TestLabel "File1" testFile1, TestLabel "File2" testFile2, TestLabel "File3" testFile3 ]