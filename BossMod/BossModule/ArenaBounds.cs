﻿namespace BossMod;
// radius is the largest horizontal/vertical dimension: radius for circle, max of width/height for rect
// note: this class to represent *relative* arena bounds (relative to arena center) - the reason being that in some cases effective center moves every frame, and bounds caches a lot (clip poly & base map for pathfinding)
// note: if arena bounds are changed, new instance is recreated; max approx error can change without recreating the instance
public abstract record class ArenaBounds(float Radius, float MapResolution, float ScaleFactor = 1)
{
    // fields below are used for clipping & drawing borders
    public readonly PolygonClipper Clipper = new();
    public float MaxApproxError;
    public RelSimplifiedComplexPolygon ShapeSimplified = new();
    public List<RelTriangle> ShapeTriangulation = [];
    private readonly PolygonClipper.Operand _clipOperand = new();
    public readonly Dictionary<object, object> Cache = [];

#pragma warning disable IDE0032
    private float _screenHalfSize;
#pragma warning restore IDE0032
    public float ScreenHalfSize
    {
        get => _screenHalfSize;
        set
        {
            if (_screenHalfSize != value)
            {
                _screenHalfSize = value;
                MaxApproxError = CurveApprox.ScreenError / value * Radius;
                ShapeSimplified = Clipper.Simplify(BuildClipPoly());
                ShapeTriangulation = ShapeSimplified.Triangulate();
                _clipOperand.Clear();
                _clipOperand.AddPolygon(ShapeSimplified); // note: I assume using simplified shape as an operand is better than raw one
            }
        }
    }

    protected abstract PolygonClipper.Operand BuildClipPoly();
    public abstract void PathfindMap(Pathfinding.Map map, WPos center);
    public abstract bool Contains(WDir offset);
    public abstract float IntersectRay(WDir originOffset, WDir dir);
    public abstract WDir ClampToBounds(WDir offset);

    // functions for clipping various shapes to bounds; all shapes are expected to be defined relative to bounds center
    public List<RelTriangle> ClipAndTriangulate(WDir[] poly) => Clipper.Intersect(new PolygonClipper.Operand((ReadOnlySpan<WDir>)poly), _clipOperand).Triangulate();
    public List<RelTriangle> ClipAndTriangulate(RelSimplifiedComplexPolygon poly) => Clipper.Intersect(new(poly), _clipOperand).Triangulate();

    public List<RelTriangle> ClipAndTriangulateCone(WDir centerOffset, float innerRadius, float outerRadius, Angle centerDirection, Angle halfAngle)
    {
        // TODO: think of a better way to do that (analytical clipping?)
        if (innerRadius >= outerRadius || innerRadius < 0 || halfAngle.Rad <= 0)
            return [];

        var fullCircle = halfAngle.Rad >= MathF.PI;
        var donut = innerRadius != 0;
        var points = (donut, fullCircle) switch
        {
            (false, false) => CurveApprox.CircleSector(outerRadius, centerDirection - halfAngle, centerDirection + halfAngle, MaxApproxError),
            (false, true) => CurveApprox.Circle(outerRadius, MaxApproxError),
            (true, false) => CurveApprox.DonutSector(innerRadius, outerRadius, centerDirection - halfAngle, centerDirection + halfAngle, MaxApproxError),
            (true, true) => CurveApprox.Donut(innerRadius, outerRadius, MaxApproxError),
        };
        var len = points.Length;
        var offset = centerOffset;
        for (var i = 0; i < len; ++i)
        {
            points[i] += offset;
        }
        return ClipAndTriangulate(points);
    }

    public List<RelTriangle> ClipAndTriangulateCircle(WDir centerOffset, float radius)
    {
        var points = CurveApprox.Circle(radius, MaxApproxError);
        var len = points.Length;
        var offset = centerOffset;
        for (var i = 0; i < len; ++i)
        {
            points[i] += offset;
        }
        return ClipAndTriangulate(points);
    }

    public List<RelTriangle> ClipAndTriangulateCapsule(WDir centerOffset, WDir direction, float radius, float length)
    {
        var points = CurveApprox.Capsule(direction, length, radius, MaxApproxError);
        var len = points.Length;
        var offset = centerOffset;
        for (var i = 0; i < len; ++i)
        {
            points[i] += offset;
        }
        return ClipAndTriangulate(points);
    }

    public List<RelTriangle> ClipAndTriangulateDonut(WDir centerOffset, float innerRadius, float outerRadius)
    {
        if (innerRadius < outerRadius && innerRadius >= 0)
        {
            var points = CurveApprox.Donut(innerRadius, outerRadius, MaxApproxError);
            var len = points.Length;
            var offset = centerOffset;
            for (var i = 0; i < len; ++i)
            {
                points[i] += offset;
            }
            return ClipAndTriangulate(points);
        }
        return [];
    }

    public List<RelTriangle> ClipAndTriangulateTri(WDir oa, WDir ob, WDir oc)
        => ClipAndTriangulate([oa, ob, oc]);

    public List<RelTriangle> ClipAndTriangulateIsoscelesTri(WDir apexOffset, WDir height, WDir halfBase)
        => ClipAndTriangulateTri(apexOffset, apexOffset + height + halfBase, apexOffset + height - halfBase);

    public List<RelTriangle> ClipAndTriangulateIsoscelesTri(WDir apexOffset, Angle direction, Angle halfAngle, float height)
    {
        var dir = direction.ToDirection();
        var normal = dir.OrthoL();
        return ClipAndTriangulateIsoscelesTri(apexOffset, height * dir, height * halfAngle.Tan() * normal);
    }

    public List<RelTriangle> ClipAndTriangulateRect(WDir originOffset, WDir direction, float lenFront, float lenBack, float halfWidth)
    {
        var side = halfWidth * direction.OrthoR();
        var front = originOffset + lenFront * direction;
        var back = originOffset - lenBack * direction;
        return ClipAndTriangulate([front + side, front - side, back - side, back + side]);
    }

    public List<RelTriangle> ClipAndTriangulateRect(WDir originOffset, Angle direction, float lenFront, float lenBack, float halfWidth)
        => ClipAndTriangulateRect(originOffset, direction.ToDirection(), lenFront, lenBack, halfWidth);

    public List<RelTriangle> ClipAndTriangulateRect(WDir startOffset, WDir endOffset, float halfWidth)
    {
        var dir = (endOffset - startOffset).Normalized();
        var side = halfWidth * dir.OrthoR();
        return ClipAndTriangulate([startOffset + side, startOffset - side, endOffset - side, endOffset + side]);
    }

    public void AddToInstanceCache(object key, object value)
    {
        if (Cache.Count > 500)
            Cache.Clear();
        Cache[key] = value;
    }
}

public sealed record class ArenaBoundsCircle(float Radius, float MapResolution = 0.5f) : ArenaBounds(Radius, MapResolution)
{
    private Pathfinding.Map? _cachedMap;

    protected override PolygonClipper.Operand BuildClipPoly() => new((ReadOnlySpan<WDir>)CurveApprox.Circle(Radius, MaxApproxError));
    public override void PathfindMap(Pathfinding.Map map, WPos center) => map.Init(_cachedMap ??= BuildMap(), center);
    public override bool Contains(WDir offset)
    {
        var radius = Radius;
        return offset.LengthSq() <= radius * radius;
    }
    public override float IntersectRay(WDir originOffset, WDir dir) => Intersect.RayCircle(originOffset, dir, Radius);

    public override WDir ClampToBounds(WDir offset)
    {
        var radius = Radius;
        if (offset.LengthSq() > radius * radius)
            offset *= radius / offset.Length();
        return offset;
    }

    private Pathfinding.Map BuildMap()
    {
        var map = new Pathfinding.Map(MapResolution, default, Radius, Radius);
        map.BlockPixelsInside2(ShapeDistance.InvertedCircle(default, Radius), -1);
        return map;
    }
}

// if rotation is 0, half-width is along X and half-height is along Z
public record class ArenaBoundsRect(float HalfWidth, float HalfHeight, Angle Rotation = default, float MapResolution = 0.5f) : ArenaBounds(Math.Max(HalfWidth, HalfHeight), MapResolution, Rotation != default ? CalculateScaleFactor(Rotation) : 1)
{
    private Pathfinding.Map? _cachedMap;
    public readonly WDir Orientation = Rotation.ToDirection();
    private static float CalculateScaleFactor(Angle Rotation)
    {
        var (sin, cos) = MathF.SinCos(Rotation.Rad);
        return Math.Abs(cos) + Math.Abs(sin);
    }

    protected override PolygonClipper.Operand BuildClipPoly() => new((ReadOnlySpan<WDir>)CurveApprox.Rect(Orientation, HalfWidth, HalfHeight));
    public override void PathfindMap(Pathfinding.Map map, WPos center) => map.Init(_cachedMap ??= BuildMap(), center);
    private Pathfinding.Map BuildMap()
    {
        var halfWidth = HalfWidth;
        var halfHeight = HalfHeight;
        var rotation = Rotation;
        var map = new Pathfinding.Map(MapResolution, default, halfWidth, halfHeight, rotation);
        map.BlockPixelsInside2(ShapeDistance.InvertedRect(default, rotation, halfHeight, halfHeight, halfWidth), -1);
        return map;
    }

    public override bool Contains(WDir offset) => offset.InRect(Orientation, HalfHeight, HalfHeight, HalfWidth);
    public override float IntersectRay(WDir originOffset, WDir dir) => Intersect.RayRect(originOffset, dir, Orientation, HalfWidth, HalfHeight);

    public override WDir ClampToBounds(WDir offset)
    {
        var orientation = Orientation;
        var halfWidth = HalfWidth;
        var halfHeight = HalfHeight;
        var offsetX = offset.Dot(orientation.OrthoL());
        var offsetY = offset.Dot(orientation);
        if (Math.Abs(offsetX) > halfWidth)
            offsetX = Math.Sign(offsetX) * halfWidth;
        if (Math.Abs(offsetY) > halfHeight)
            offsetY = Math.Sign(offsetY) * halfHeight;
        return orientation.OrthoL() * offsetX + orientation * offsetY;
    }
}

public sealed record class ArenaBoundsSquare(float Radius, Angle Rotation = default, float MapResolution = 0.5f) : ArenaBoundsRect(Radius, Radius, Rotation, MapResolution) { }

// custom complex polygon bounds
public record class ArenaBoundsCustom : ArenaBounds
{
    private Pathfinding.Map? _cachedMap;
    public readonly RelSimplifiedComplexPolygon poly;
    private readonly (WDir, WDir)[] edges;
    public float HalfWidth, HalfHeight;

    public ArenaBoundsCustom(float Radius, RelSimplifiedComplexPolygon Poly, float MapResolution = 0.5f, float ScaleFactor = 1)
        : base(Radius, MapResolution, ScaleFactor)
    {
        poly = Poly;

        var edgeList = new List<(WDir, WDir)>();
        var count = Poly.Parts.Count;
        for (var i = 0; i < count; ++i)
        {
            var part = Poly.Parts[i];
            edgeList.AddRange(part.ExteriorEdges);
            var len = part.Holes.Length;
            if (len == 0)
                continue;
            for (var j = 0; j < len; ++j)
            {
                edgeList.AddRange(part.InteriorEdges(j));
            }
        }
        edges = [.. edgeList];
    }

    protected override PolygonClipper.Operand BuildClipPoly() => new(poly);
    public override void PathfindMap(Pathfinding.Map map, WPos center) => map.Init(_cachedMap ??= BuildMap(), center);

    public override bool Contains(WDir offset) => poly.Contains(offset);

    public override float IntersectRay(WDir originOffset, WDir dir)
    {
        var cacheKey = (poly, originOffset, dir);
        if (Cache.TryGetValue(cacheKey, out var cachedResult))
            return (float)cachedResult;
        var result = Intersect.RayPolygon(originOffset, dir, poly);
        AddToInstanceCache(cacheKey, result);
        return result;
    }

    public override WDir ClampToBounds(WDir offset)
    {
        var cacheKey = (poly, offset);
        if (Cache.TryGetValue(cacheKey, out var cachedResult))
            return (WDir)cachedResult;
        if (Contains(offset) || offset.AlmostEqual(default, 1f) || Math.Abs(offset.X) < 0.1f) // if actor is almost in the center of the arena, do nothing (eg donut arena)
        {
            Cache[(poly, offset)] = offset;
            return offset;
        }
        var minDistance = float.MaxValue;
        var nearestPoint = offset;
        var len = edges.Length;
        for (var i = 0; i < len; ++i)
        {
            ref var edge = ref edges[i];
            var edge1 = edge.Item1;
            var segmentVector = edge.Item2 - edge1;
            var t = Math.Max(0, Math.Min(1, (offset - edge1).Dot(segmentVector) / segmentVector.LengthSq()));
            var nearest = edge1 + t * segmentVector;
            var distance = (nearest - offset).LengthSq();

            if (distance < minDistance)
            {
                minDistance = distance;
                nearestPoint = nearest;
            }
        }

        AddToInstanceCache(cacheKey, nearestPoint);
        return nearestPoint;
    }

    private Pathfinding.Map BuildMap()
    {
        var polygon = poly;
        if (HalfHeight == default) // calculate bounding box if not already done by ArenaBoundsComplex to reduce amount of point in polygon tests
        {
            float minX = float.MaxValue, maxX = float.MinValue, minZ = float.MaxValue, maxZ = float.MinValue;
            var count = polygon.Parts.Count;
            for (var i = 0; i < count; ++i)
            {
                var part = polygon.Parts[i];
                var len = part.Exterior.Length;
                for (var j = 0; j < len; ++j)
                {
                    var vertex = part.Exterior[j];
                    var vertexX = vertex.X;
                    var vertexZ = vertex.Z;
                    if (vertex.X < minX)
                        minX = vertexX;
                    if (vertex.X > maxX)
                        maxX = vertexX;
                    if (vertex.Z < minZ)
                        minZ = vertexZ;
                    if (vertex.Z > maxZ)
                        maxZ = vertexZ;
                }
            }
            HalfWidth = (maxX - minX) * 0.5f;
            HalfHeight = (maxZ - minZ) * 0.5f;
        }
        var map = new Pathfinding.Map(MapResolution, default, HalfWidth, HalfHeight);
        var pixels = map.PixelMaxG;
        var width = map.Width;
        var height = map.Height;
        var resolution = map.Resolution;
        map.BlockPixelsInside(new PolygonWithHolesDistanceFunction(default, poly).InvertedDistance, -1, 0.49999f * resolution); // check inner circle of the pixel
        // now check the corners
        var halfSample = resolution * 0.49999f; // tiny offset to account for floating point inaccuracies

        WDir[] sampleOffsets =
        [
        new(-halfSample, -halfSample),
        new(-halfSample,  halfSample),
        new(halfSample, -halfSample),
        new(halfSample, halfSample)
        ];

        var dx = new WDir(resolution, 0f);
        var dy = new WDir(0f, resolution);
        var startPos = map.Center - ((width >> 1) - 0.5f) * dx - ((height >> 1) - 0.5f) * dy;

        Parallel.For(0, height, y =>
        {
            var rowOffset = y * width;
            var posY = startPos + y * dy;
            for (var x = 0; x < width; ++x)
            {
                ref var pixel = ref pixels[rowOffset + x];
                if (pixel == -1)
                    continue;
                var pos = posY + x * dx;

                var relativeCenter = new WDir(pos.X, pos.Z);
                var allInside = true;

                for (var i = 0; i < 4; ++i)
                {
                    if (!polygon.Contains(relativeCenter + sampleOffsets[i]))
                    {
                        allInside = false;
                        break;
                    }
                }
                pixel = allInside ? float.MaxValue : -1f;
            }
        });

        return map;
    }
}

// for creating complex bounds by using two IEnumerable of shapes
// first IEnumerable contains platforms that will be united, second optional IEnumberale contains shapes that will be subtracted
// for convenience third list will optionally perform additional unions at the end
public sealed record class ArenaBoundsComplex : ArenaBoundsCustom
{
    public readonly WPos Center;
    public bool IsCircle; // can be used by gaze component for gazes outside of the arena

    public ArenaBoundsComplex(Shape[] UnionShapes, Shape[]? DifferenceShapes = null, Shape[]? AdditionalShapes = null, float MapResolution = 0.5f, float ScaleFactor = 1)
        : base(BuildBounds(UnionShapes, DifferenceShapes, AdditionalShapes, MapResolution, ScaleFactor, out var center, out var halfWidth, out var halfHeight))
    {
        Center = center;
        HalfWidth = halfWidth;
        HalfHeight = halfHeight;
    }

    private static ArenaBoundsCustom BuildBounds(Shape[] unionShapes, Shape[]? differenceShapes, Shape[]? additionalShapes, float mapResolution, float scalefactor, out WPos center, out float halfWidth, out float halfHeight)
    {
        var properties = CalculatePolygonProperties(unionShapes, differenceShapes ?? [], additionalShapes ?? []);
        center = properties.Center;
        halfWidth = properties.HalfWidth;
        halfHeight = properties.HalfHeight;
        return new(scalefactor == 1 ? properties.Radius : properties.Radius / scalefactor, properties.Poly, mapResolution, scalefactor);
    }

    private static (WPos Center, float HalfWidth, float HalfHeight, float Radius, RelSimplifiedComplexPolygon Poly) CalculatePolygonProperties(Shape[] unionShapes, Shape[] differenceShapes, Shape[] additionalShapes)
    {
        var unionPolygons = ParseShapes(unionShapes);
        var differencePolygons = ParseShapes(differenceShapes);
        var additionalPolygons = ParseShapes(additionalShapes);

        var combinedPoly = CombinePolygons(unionPolygons, differencePolygons, additionalPolygons);

        float minX = float.MaxValue, maxX = float.MinValue, minZ = float.MaxValue, maxZ = float.MinValue;

        for (var i = 0; i < combinedPoly.Parts.Count; ++i)
        {
            var part = combinedPoly.Parts[i];
            for (var j = 0; j < part.Exterior.Length; ++j)
            {
                var vertex = part.Exterior[j];
                if (vertex.X < minX)
                    minX = vertex.X;
                if (vertex.X > maxX)
                    maxX = vertex.X;
                if (vertex.Z < minZ)
                    minZ = vertex.Z;
                if (vertex.Z > maxZ)
                    maxZ = vertex.Z;
            }
        }

        var center = new WPos((minX + maxX) * 0.5f, (minZ + maxZ) * 0.5f);
        var maxDistX = Math.Max(Math.Abs(maxX - center.X), Math.Abs(minX - center.X));
        var maxDistZ = Math.Max(Math.Abs(maxZ - center.Z), Math.Abs(minZ - center.Z));
        var halfWidth = (maxX - minX) * 0.5f;
        var halfHeight = (maxZ - minZ) * 0.5f;

        var combinedPolyCentered = CombinePolygons(ParseShapes(unionShapes, center), ParseShapes(differenceShapes, center), ParseShapes(additionalShapes, center));
        return (center, halfWidth, halfHeight, Math.Max(maxDistX, maxDistZ), combinedPolyCentered);
    }

    private static RelSimplifiedComplexPolygon CombinePolygons(RelSimplifiedComplexPolygon[] unionPolygons, RelSimplifiedComplexPolygon[] differencePolygons, RelSimplifiedComplexPolygon[] secondUnionPolygons)
    {
        var clipper = new PolygonClipper();
        var operandUnion = new PolygonClipper.Operand();
        var operandDifference = new PolygonClipper.Operand();
        var operandSecondUnion = new PolygonClipper.Operand();

        for (var i = 0; i < unionPolygons.Length; ++i)
            operandUnion.AddPolygon(unionPolygons[i]);
        for (var i = 0; i < differencePolygons.Length; ++i)
            operandDifference.AddPolygon(differencePolygons[i]);
        for (var i = 0; i < secondUnionPolygons.Length; ++i)
            operandSecondUnion.AddPolygon(secondUnionPolygons[i]);

        var combinedShape = clipper.Difference(operandUnion, operandDifference);
        if (secondUnionPolygons.Length != 0)
            combinedShape = clipper.Union(new PolygonClipper.Operand(combinedShape), operandSecondUnion);

        return combinedShape;
    }

    private static RelSimplifiedComplexPolygon[] ParseShapes(Shape[] shapes, WPos center = default)
    {
        var lenght = shapes.Length;
        var polygons = new RelSimplifiedComplexPolygon[lenght];
        for (var i = 0; i < lenght; ++i)
        {
            polygons[i] = shapes[i].ToPolygon(center);
        }
        return polygons;
    }
}
