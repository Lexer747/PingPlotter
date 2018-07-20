module Constants where

import InternalGraph


demoGraph = Graph {maxX = 100, minX=0, maxY = 100, minY=0,title="test",
        dataSet= [(1,1),(11,9),(21,21),(31,28),(49,32),(51,53),(65,60),(74,69),(80,88),(92,99)]
    }
    
demoInternal = toInternal demoGraph

scaledDemo = fitToScreen demoInternal