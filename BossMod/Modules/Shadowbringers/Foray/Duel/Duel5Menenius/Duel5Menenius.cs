﻿namespace BossMod.Shadowbringers.Foray.Duel.Duel5Menenius;

class SpiralScourge(BossModule module) : Components.SingleTargetCast(module, (uint)AID.SpiralScourge, "Use Manawall, Excellence, or Invuln.");
class CallousCrossfire(BossModule module) : Components.SingleTargetCast(module, (uint)AID.CallousCrossfire, "Use Light Curtain / Reflect.");

class ReactiveMunition(BossModule module) : Components.StayMove(module)
{
    public override void OnStatusGain(Actor actor, ActorStatus status)
    {
        if ((SID)status.ID is SID.AccelerationBomb)
        {
            if (Raid.FindSlot(actor.InstanceID) is var slot && slot >= 0)
                PlayerStates[slot] = new(Requirement.Stay, status.ExpireAt);
        }
    }

    public override void OnStatusLose(Actor actor, ActorStatus status)
    {
        if ((SID)status.ID is SID.AccelerationBomb)
        {
            if (Raid.FindSlot(actor.InstanceID) is var slot && slot >= 0)
                PlayerStates[slot] = default;
        }
    }
}

class SenseWeakness(BossModule module) : Components.StayMove(module)
{
    public override void OnCastStarted(Actor caster, ActorCastInfo spell)
    {
        if ((AID)spell.Action.ID == AID.SenseWeakness)
        {
            if (Raid.FindSlot(caster.TargetID) is var slot && slot >= 0)
                PlayerStates[slot] = new(Requirement.Move, Module.CastFinishAt(spell));
        }
    }

    public override void OnEventCast(Actor caster, ActorCastEvent spell)
    {
        if ((AID)spell.Action.ID == AID.SenseWeakness)
        {
            if (Raid.FindSlot(caster.TargetID) is var slot && slot >= 0)
                PlayerStates[slot] = default;
        }
    }
}

class MagitekImpetus(BossModule module) : Components.StatusDrivenForcedMarch(module, 3f, (uint)SID.ForwardMarch, (uint)SID.AboutFace, (uint)SID.LeftFace, (uint)SID.RightFace, activationLimit: 1);
class ProactiveMunition(BossModule module) : Components.StandardChasingAOEs(module, 6f, (uint)AID.ProactiveMunitionTrackingStart, (uint)AID.ProactiveMunitionTrackingMove, 6, 1, 5);

[ModuleInfo(BossModuleInfo.Maturity.Contributed, Contributors = "SourP", GroupType = BossModuleInfo.GroupType.BozjaDuel, GroupID = 778, NameID = 23)] // bnpcname=9695
public class Duel5Menenius(WorldState ws, Actor primary) : BossModule(ws, primary, new(-810f, 520f), new ArenaBoundsSquare(20f))
{
    protected override bool CheckPull() => base.CheckPull() && Raid.Player()!.Position.InSquare(Arena.Center, 20f);
}
