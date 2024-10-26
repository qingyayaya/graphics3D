# ShadowFigure

ShadowFigure 是用来在 Mathematica 里画投影图的一个包，比如运行：

```mathematica
ShadowFigure[ PolyhedronData["Spikey"] ]
```

将得到：

<div align="center"><img src="./pics/test1.png" width="400"/></div>

# Graphics3D

修复后的 Mathematica 的 Graphics\`Graphics3D\` 包，可以兼容高版本的 Mathematica，但是并不推荐使用，最好使用 ShadowFigure 包。

# 使用方法

对于 ShadowFigure 包，使用之前可以直接用下面的代码进行加载：

```mathematica
Get["https://github.com/qingyayaya/graphics3D/raw/master/ShadowFigure.m"]
```

对于 Graphics\`Graphics3D\` 包，用下面的代码加载：

```mathematica
Get["https://github.com/qingyayaya/graphics3D/raw/master/Graphics3D.m"]
```

