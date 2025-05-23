namespace BossMod.Shadowbringers.Foray.DelubrumReginae;

public abstract class TrinitySeeker(WorldState ws, Actor primary) : BossModule(ws, primary, StartingArena.Center, StartingArena)
{
    private static readonly WPos ArenaCenter = new(0, 278);
    private static readonly DonutSegmentV[] barricades = [.. GenerateBarricades()];

    private static DonutSegmentV[] GenerateBarricades()
    {
        var barricades = new DonutSegmentV[12];
        var a22 = 22.5f.Degrees();
        var a45 = 45.Degrees();
        var a90 = 90.Degrees();
        var a5 = 5.Degrees();
        var a26 = 26.1f.Degrees();
        var a63 = 63.9f.Degrees();
        const float innerRadius = 18.7f;
        const float outerRadius = 21.6f;
        var index = 0;
        for (var i = 0; i < 4; ++i)
        {
            var ai = a90 * i;
            barricades[index++] = new(ArenaCenter, innerRadius, outerRadius, a45 + ai, a22, 6); // each donut segment got 6 inner and 6 outer edges
            // side cushions for hitbox radius
            barricades[index++] = new(ArenaCenter, innerRadius, outerRadius, a26 + ai, a5, 2);
            barricades[index++] = new(ArenaCenter, innerRadius, outerRadius, a63 + ai, a5, 2);
        }
        return barricades;
    }

    public static readonly ArenaBoundsComplex StartingArena = new([new Polygon(ArenaCenter, 29.5f, 48)], [.. barricades, new Rectangle(new(default, 248f), 20f, 1.25f), new Rectangle(new(default, 308f), 20f, 1.43f)]);
    public static readonly ArenaBoundsComplex DefaultArena = new([new Polygon(ArenaCenter, 25f, 48)], barricades);
}
