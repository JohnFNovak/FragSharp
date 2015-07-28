using System;
using System.Collections.Generic;

using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Content;
using Microsoft.Xna.Framework.Graphics;

namespace FragSharpFramework
{
    public enum TranslationType { ReplaceMember, ReplaceExpression, UnderscoreAppend, ReverseArguments };

    public class Vals
    {
        [Vals(true, false)] public class BoolAttribute : Attribute { }
        public static readonly bool[] Bool = new bool[] { true, false };
    }

    public class ValsAttribute : Attribute
    {
        public ValsAttribute(params int[] vals) { }
        public ValsAttribute(params float[] vals) { }
        public ValsAttribute(params bool[] vals) { }
    }

    public enum CastStyle { ExplicitCasts, ImplicitCast, NoCasts };
    public class CopyAttribute : Attribute
    {
        public CopyAttribute(Type type) { }
        public CopyAttribute(Type type, CastStyle style) { }
    }

    public class KeepInCopy : Attribute { }

    public enum Special { rgba_hex, rgb_hex }
    public class SpecialAttribute : Attribute
    {
        public SpecialAttribute(Special name) { }
    }

    public class __slAttribute : Attribute
    {
        public __slAttribute() { }
        public __slAttribute(string translation) { }
        public __slAttribute(string translation, TranslationType translation_type) { }
    }

    public class HlslAttribute : __slAttribute
    {
        public HlslAttribute() : base() { }
        public HlslAttribute(string translation) : base(translation) { }
        public HlslAttribute(string translation, TranslationType translation_type) : base(translation, translation_type) { }
    }

    public class GlslAttribute : __slAttribute
    {
        public GlslAttribute() : base() { }
        public GlslAttribute(string translation) : base(translation) { }
        public GlslAttribute(string translation, TranslationType translation_type) : base(translation, translation_type) { }
    }

    public class POSITION0Attribute : Attribute { }
    public class COLOR0Attribute : Attribute { }
    public class TEXCOORD0Attribute : Attribute { }

    public class VertexShaderAttribute : Attribute { }
    public class FragmentShaderAttribute : Attribute { }
}