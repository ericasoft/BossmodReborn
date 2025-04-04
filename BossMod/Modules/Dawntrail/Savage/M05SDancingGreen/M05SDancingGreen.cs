namespace BossMod.Dawntrail.Savage.M05SDancingGreen;

class EighthBeats(BossModule module) : Components.SpreadFromCastTargets(module, ActionID.MakeSpell(AID.EighthBeats), 5f);
class QuarterBeats(BossModule module) : Components.StackWithCastTargets(module, ActionID.MakeSpell(AID.QuarterBeats), 4f, 2, 2);

class DeepCut(BossModule module) : Components.BaitAwayIcon(module, new AOEShapeCone(60, 22.5f.Degrees()), (uint)IconID.DeepCut, ActionID.MakeSpell(AID.DeepCut), 5f, tankbuster: true);

[ModuleInfo(BossModuleInfo.Maturity.Verified, Contributors = "The Combat Reborn Team (Malediktus)", GroupType = BossModuleInfo.GroupType.CFC, GroupID = 1020, NameID = 13778)]
public class M05SDancingGreen(WorldState ws, Actor primary) : BossModule(ws, primary, new(100f, 100f), new ArenaBoundsSquare(20));
