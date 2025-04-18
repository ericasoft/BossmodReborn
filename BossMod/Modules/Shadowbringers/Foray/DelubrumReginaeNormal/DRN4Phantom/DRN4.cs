﻿namespace BossMod.Shadowbringers.Foray.DelubrumReginae.Normal.DRN4Phantom;

class UndyingHatred(BossModule module) : Components.SimpleKnockbacks(module, (uint)AID.UndyingHatred, 30, kind: Kind.DirForward);
class VileWave(BossModule module) : Components.SimpleAOEs(module, (uint)AID.VileWave, new AOEShapeCone(45, 60.Degrees()));

[ModuleInfo(BossModuleInfo.Maturity.WIP, Contributors = "The Combat Reborn Team", GroupType = BossModuleInfo.GroupType.CFC, GroupID = 760, NameID = 9755)]
public class DRN4Phantom(WorldState ws, Actor primary) : BossModule(ws, primary, new(202, -370), new ArenaBoundsSquare(24));
