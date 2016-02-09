import Card
import Field

main = do
    rnd1 <- randCard
    rnd2 <- randCard
    rnd3 <- randCard
    let tree = initiation rnd1 0
        tree2 = insertCard 1 rnd2 1 tree
        tree3 = insertCard 6 rnd2 1 tree2
        tree4 = refreshTree tree3 [tree3] (Size 6 4)

    print $ take 4 $ unusedPassage tree4
    print tree4
