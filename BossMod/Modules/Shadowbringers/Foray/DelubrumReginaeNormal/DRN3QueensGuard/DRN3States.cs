﻿namespace BossMod.Shadowbringers.Foray.DelubrumReginae.DRN3QueensGuard;

class DRN3QueensGuardStates : StateMachineBuilder
{
    public DRN3QueensGuardStates(BossModule module) : base(module)
    {
        TrivialPhase()
            .ActivateOnEnter<BloodAndBoneQueenShotUnseen>()
            .ActivateOnEnter<RapidSeverShotInTheDark>()
            .ActivateOnEnter<Enrages>()
            .ActivateOnEnter<OptimalPlaySword>()
            .ActivateOnEnter<OptimalPlayShield>()
            .ActivateOnEnter<CoatOfArms>()
            .ActivateOnEnter<AboveBoard>()
            .ActivateOnEnter<TurretsTour>()
            .ActivateOnEnter<PawnOff>();
    }
}
