﻿namespace BossMod.Dawntrail.Raid.M02NHoneyBLovely;

class CallMeHoney(BossModule module) : Components.RaidwideCast(module, (uint)AID.CallMeHoney);

abstract class TemptingTwist(BossModule module, uint aid) : Components.SimpleAOEs(module, aid, new AOEShapeDonut(7f, 30f));
class TemptingTwist1(BossModule module) : TemptingTwist(module, (uint)AID.TemptingTwist1);
class TemptingTwist2(BossModule module) : TemptingTwist(module, (uint)AID.TemptingTwist2);

abstract class HoneyBeeline(BossModule module, uint aid) : Components.SimpleAOEs(module, aid, new AOEShapeRect(60f, 7f));
class HoneyBeeline1(BossModule module) : HoneyBeeline(module, (uint)AID.HoneyBeeline1);
class HoneyBeeline2(BossModule module) : HoneyBeeline(module, (uint)AID.HoneyBeeline2);

class HoneyedBreeze(BossModule module) : Components.BaitAwayIcon(module, new AOEShapeCone(40f, 15f.Degrees()), (uint)IconID.HoneyedBreezeTB, (uint)AID.HoneyedBreeze, 5f, tankbuster: true);

class HoneyBLive(BossModule module) : Components.RaidwideCastDelay(module, (uint)AID.HoneyBLiveVisual, (uint)AID.HoneyBLive, 8.3f);
class Heartsore(BossModule module) : Components.SpreadFromCastTargets(module, (uint)AID.Heartsore, 6f);
class Heartsick(BossModule module) : Components.StackWithCastTargets(module, (uint)AID.Heartsick, 6f, 4, 4);
class Loveseeker(BossModule module) : Components.SimpleAOEs(module, (uint)AID.Loveseeker, 10f);
class BlowKiss(BossModule module) : Components.SimpleAOEs(module, (uint)AID.BlowKiss, new AOEShapeCone(40f, 60f.Degrees()));
class HoneyBFinale(BossModule module) : Components.RaidwideCast(module, (uint)AID.HoneyBFinale);
class DropOfVenom(BossModule module) : Components.StackWithCastTargets(module, (uint)AID.DropOfVenom, 6f, 8, 8);
class SplashOfVenom(BossModule module) : Components.SpreadFromCastTargets(module, (uint)AID.SplashOfVenom, 6f);

class BlindingLove1 : Components.SimpleAOEs
{
    public BlindingLove1(BossModule module) : base(module, (uint)AID.BlindingLove1, new AOEShapeRect(50f, 4f)) { MaxDangerColor = 2; }
}
class BlindingLove2(BossModule module) : Components.SimpleAOEs(module, (uint)AID.BlindingLove2, new AOEShapeRect(50f, 4f));
class HeartStruck1(BossModule module) : Components.SimpleAOEs(module, (uint)AID.HeartStruck1, 4f);
class HeartStruck2(BossModule module) : Components.SimpleAOEs(module, (uint)AID.HeartStruck2, 6f);
class HeartStruck3(BossModule module) : Components.SimpleAOEs(module, (uint)AID.HeartStruck3, 10f, maxCasts: 8);

class Fracture(BossModule module) : Components.CastTowers(module, (uint)AID.Fracture, 4f)
{
    public override void Update()
    {
        var count = Towers.Count;
        if (count == 0)
            return;
        var party = Raid.WithoutSlot(false, true, true);
        var len = party.Length;
        BitMask forbidden = new();
        for (var i = 0; i < len; ++i)
        {
            ref readonly var statuses = ref party[i].Statuses;
            var lenStatuses = statuses.Length;
            for (var j = 0; j < lenStatuses; ++j)
            {
                if (statuses[j].ID is ((uint)SID.HeadOverHeels) or ((uint)SID.HopelessDevotion))
                {
                    forbidden[i] = true;
                }
            }
        }
        var towers = CollectionsMarshal.AsSpan(Towers);
        for (var i = 0; i < count; ++i)
        {
            ref var t = ref towers[i];
            t.ForbiddenSoakers = forbidden;
        }
    }
}

[ModuleInfo(BossModuleInfo.Maturity.Verified, Contributors = "The Combat Reborn Team (Malediktus, LTS)", GroupType = BossModuleInfo.GroupType.CFC, GroupID = 987, NameID = 12685)]
public class M02NHoneyBLovely(WorldState ws, Actor primary) : BossModule(ws, primary, new(100f, 100f), new ArenaBoundsCircle(20f));
