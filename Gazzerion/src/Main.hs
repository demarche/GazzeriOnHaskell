import Card
import Field

main = do
    rnd1 <- randCard
    rnd2 <- randCard
    rnd3 <- randCard
    let tree = initiation rnd1 (State 0 0 0)
        tree2 = insertCard 2 rnd2 (State 0 0 1) tree
        tree3 = insertCard 6 rnd3 (State 0 0 1) tree2

    print $ take 4 $ unusedPassage tree3
    print tree3
