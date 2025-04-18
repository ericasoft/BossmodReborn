﻿namespace BossMod.Dawntrail.Extreme.Ex4Zelenia;

[ConfigDisplay(Order = 0x140, Parent = typeof(DawntrailConfig))]
public class Ex4ZeleniaConfig() : ConfigNode()
{
    [PropertyDisplay("Dangerous rose tile color:")]
    public Color RoseTileColor = new(0x802929A1);
}
