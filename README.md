# ShadowFigure

ShadowFigure是用来在Mathematica里画投影图的一个包，比如运行：

```
ShadowFigure[ PolyhedronData["Spikey"] ]
```

将得到：

<div align="center"><img src="./pics/test1.png" width="400"/></div>

# Graphics3D

修复后的Mathematica的Graphics$\grave{}$Graphics3D$\grave{}$ 包，修复后可以兼容高版本的Mathematica，但是并不推荐使用，最好使用ShadowFigure包。

# 使用方法

对于ShadowFigure包，使用之前可以直接用下面的代码进行加载：

```
Get["https://gitee.com/qingyaya/graphics3D/raw/master/ShadowFigure.m"]
```

对于Graphics$\grave{}$Graphics3D$\grave{}$ 包，用下面的代码加载：

```
Get["https://gitee.com/qingyaya/graphics3D/raw/master/Graphics3D.m"]
```

