using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Diagnostics;

using Roslyn.Compilers;
using Roslyn.Compilers.Common;
using Roslyn.Services;
using Roslyn.Compilers.CSharp;

using FragSharp.Build;
using FragSharpFramework;

using Microsoft.Xna.Framework.Graphics;
using Microsoft.Xna.Framework.Content.Pipeline;
using Microsoft.Xna.Framework.Content.Pipeline.Graphics;
using Microsoft.Xna.Framework.Content.Pipeline.Processors;

namespace FragSharp
{
    /// <summary>
    /// A public class for runtime options like shader language
    /// </summary>
    public class Options
    {
        public static string ShaderLanguage = "Hlsl";
    }

    public static class Assert
    {
        public class AssertFail : Exception { public AssertFail() { } }

        public static void That(bool expression)
        {
            if (!expression)
            {
                throw new AssertFail();
            }
        }
    }

    static class ListExtension
    {
        public static void AddUnique<T>(this List<T> list, T item)
        {
            if (!list.Contains(item)) list.Add(item);
        }

        public static void AddUnique<T>(this List<T> list1, List<T> list2)
        {
            list1.AddRange(list2.Distinct().Except(list1));
        }
    }

    // NOTE: there is another TranslationType enum defined in FragSharpFramework Attributes.cs and it seems to be disconnected from this
    /// <summary>
    /// The set of possible ways that an expression can be translated to shader language.
    /// </summary>
    public enum TranslationType { ReplaceMember, ReplaceExpression, UnderscoreAppend, ReverseArguments };
    public struct MapInfo
    {
        public string Translation;
        public TranslationType TranslationType;

        public MapInfo(string translation, TranslationType translation_type = TranslationType.ReplaceMember)
        {
            this.Translation = translation;
            this.TranslationType = translation_type;
        }
    }

    /// <summary>
    /// Public class which maintains expression translations from C# to shader langauges
    /// </summary>
    public static class TranslationLookup
    {
        public static Dictionary<Symbol, MapInfo> SymbolMap = new Dictionary<Symbol, MapInfo>();

        /// <summary>
        /// Looks up a symbol in SymbolMap recusively. Returns null if not found
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns></returns>
        public static MapInfo RecursiveLookup(TypeSymbol symbol)
        {
            if (SymbolMap.ContainsKey(symbol))
            {
                return SymbolMap[symbol];
            }
            else
            {
                if (symbol.BaseType != null)
                    return RecursiveLookup(symbol.BaseType);
            }

            return new MapInfo(null);
        }

        static void ProcessAccessor(TypeDeclarationSyntax type, AccessorDeclarationSyntax accessor)
        {
            Console.Write(0);
        }

        /// <summary>
        /// Processes typemap, adding members to SymbolMap.
        /// </summary>
        /// <param name="typemap"></param>
        static void ProcessTypeMap(NamedTypeSymbol typemap)
        {
            var members = typemap.GetMembers();

            foreach (var member in members)
            {
                // GetAttribute should get the attribute corresponding to whichever shader language we are using.
                var attribute = member.GetAttribute(Options.ShaderLanguage);
                if (attribute != null)
                {
                    // Get single argument to this function. It will serve as the key in the map.
                    var method = member as MethodSymbol;
                    if (null != method)
                    {
                        var type = method.Parameters.First();
                        CreateMapEntry(type.Type, attribute);
                    }
                }
            }
        }

        /// <summary>
        /// Processes Nodes, adding attributes to SymbolMap, unless attribute is TypeMap, then it adds the attributes members to SymbolMap.
        /// </summary>
        /// <param name="Models"></param>
        /// <param name="Nodes"></param>
        public static void ProcessTypes(Dictionary<SyntaxTree, SemanticModel> Models, IEnumerable<SyntaxNode> Nodes)
        {
            var classes = Nodes.OfType<TypeDeclarationSyntax>();

            foreach (var _class in classes)
            {
                var symbol = Models[_class.SyntaxTree].GetDeclaredSymbol(_class);

                if (SymbolMap.ContainsKey(symbol)) continue;

                if (symbol.Name == "__TypeMaps")
                {
                    ProcessTypeMap(symbol);
                    continue;
                }

                var attribute = symbol.GetAttribute(Options.ShaderLanguage);

                if (attribute != null)
                {
                    CreateMapEntry(symbol, attribute);
                }
            }
        }

        /// <summary>
        /// Adds members to SymbolMap along with translation info from shader langague attributes
        /// </summary>
        /// <param name="member"></param>
        /// <param name="attribute"></param>
        private static void CreateMapEntry(Symbol member, AttributeData attribute)
        {
            var args = attribute.ConstructorArguments.ToList();
            if (args.Count > 1)
            {
                SymbolMap.Add(member, new MapInfo(args[0].Value.ToString(), (TranslationType)args[1].Value));
            }
            else if (args.Count > 0)
            {
                SymbolMap.Add(member, new MapInfo(args[0].Value.ToString()));
            }
            else
            {
                SymbolMap.Add(member, new MapInfo(member.Name));
            }
        }

        /// <summary>
        /// Processes all nodes which are classes, looking them up in Models, and adding their members to SymbolMap.
        /// </summary>
        /// <param name="Models"></param>
        /// <param name="Nodes"></param>
        public static void ProcessMembers(Dictionary<SyntaxTree, SemanticModel> Models, IEnumerable<SyntaxNode> Nodes)
        {
            var classes = Nodes.OfType<TypeDeclarationSyntax>();
            HashSet<Symbol> Processed = new HashSet<Symbol>(); // Keep track of which classes have been processed so we don't double-process.
            
            foreach (var _class in classes)
            {
                var symbol = Models[_class.SyntaxTree].GetDeclaredSymbol(_class);

                if (Processed.Contains(symbol)) continue;

                Processed.Add(symbol);
                
                var members = symbol.GetMembers();

                foreach (var member in members)
                {
                    if (member is NamedTypeSymbol) continue; // Skip nested type defintions. We alraedy processed all types.

                    var attribute = member.GetAttribute(Options.ShaderLanguage);
                    if (attribute != null)
                    {
                        CreateMapEntry(member, attribute);
                    }
                }
            }
        }

        /// <summary>
        /// Loops through Nodes which are classes, gets their members. Loops through members. 
        /// If member is as definition, a FieldSymbol, and is readonly and has a non-null declaration initializer value it's translation is looked up. 
        /// If that is non-null it is added to SymbolMap. 
        /// </summary>
        /// <param name="Models"></param>
        /// <param name="Nodes"></param>
        /// <param name="Compilation"></param>
        public static void ProcessReadonlys(Dictionary<SyntaxTree, SemanticModel> Models, IEnumerable<SyntaxNode> Nodes, Compilation Compilation)
        {
            var classes = Nodes.OfType<TypeDeclarationSyntax>();

            foreach (var _class in classes)
            {
                var symbol = Models[_class.SyntaxTree].GetDeclaredSymbol(_class);
                var members = symbol.GetMembers();

                foreach (var member in members)
                {
                    if (!SymbolMap.ContainsKey(member) && member.IsDefinition && member is FieldSymbol)
                    {
                        var field = (FieldSymbol)member;

                        if (field.ToString().Contains("ColorArray")) Console.Write("!");

                        if (field.IsReadOnly)
                        {
                            var decleration = member.DeclaringSyntaxNodes.First() as VariableDeclaratorSyntax;

                            if (decleration.Initializer == null) continue; // Skip readonly's that have no compile time values.

                            var creation = decleration.Initializer.Value;// as ObjectCreationExpressionSyntax;
                            if (null != creation)
                            {
                                var constructor_info = Models[creation.SyntaxTree].GetSymbolInfo(creation);
                                var constructor = constructor_info.Symbol;

                                string translation = null;
                                try
                                {
                                    //if (constructor != null)// && SymbolMap.ContainsKey(constructor))
                                    if (Options.ShaderLanguage == "Hlsl")
                                    {
                                        var writer = new HlslWriter(Models, Compilation);

                                        writer.CompileExpression(creation);
                                        translation = writer.GetString();
                                    }
                                    else if (Options.ShaderLanguage == "Glsl")
                                    {
                                        var writer = new GlslWriter(Models, Compilation);

                                        writer.CompileExpression(creation);
                                        translation = writer.GetString();
                                    }
                                }
                                catch (Exception e)
                                {
                                    
                                }

                                if (translation != null)
                                {
                                    SymbolMap.Add(member, new MapInfo(translation, TranslationType.ReplaceExpression));
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /// <summary>
    /// Public class for holding filepaths for writing out generates and compiled shaders.
    /// </summary>
    class Paths
    {
        public readonly string
            CompilerDir, FrameworkDir, ProjectDir, TargetDir, BoilerRoot, ShaderCompileDir, ShaderBuildDir, ProjectPath,
            Configuration;


        public Paths(string[] args)
        {
            ProjectPath = args[0];
            TargetDir = args[1];
            Configuration = args[2];

            ProjectDir = Path.GetDirectoryName(ProjectPath);

            BoilerRoot = Path.Combine(ProjectDir, "__FragSharp");
            ShaderCompileDir = Path.Combine(ProjectDir, "__GeneratedShaders");
            ShaderBuildDir = Path.Combine(Path.Combine(TargetDir, "Content"), "FragSharpShaders");

            CompilerDir  = Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location);
            FrameworkDir = Path.Combine("FragSharpFramework", CompilerDir);
        }
    }

    /// <summary>
    /// Class which corresponds to a specific shader file. Infers spcializations (possible paths of branching logic) and generates shader code and boilerplate.
    /// </summary>
    class ShaderClass : RoslynHelper
    {
        public static List<ShaderClass> Shaders = new List<ShaderClass>();

        public string TargetFile;

        static Dictionary<NamedTypeSymbol, ShaderClass> SymbolLookup = new Dictionary<NamedTypeSymbol, ShaderClass>();
        //static Dictionary<MethodDeclarationSyntax, HlslShaderWriter>
        //    VertexCompilations   = new Dictionary<MethodDeclarationSyntax, HlslShaderWriter>(),
        //    FragmentCompilations = new Dictionary<MethodDeclarationSyntax, HlslShaderWriter>();

        Dictionary<SyntaxTree, SemanticModel> Models;
        Compilation SourceCompilation;

        /// <summary>
        /// Checks if SymbolLookup contains symbol. If not found it creates a new ShaderClass from symbol, Models, and SourceCompilation and adds it to SymbolLookup as well as Shaders.
        /// </summary>
        /// <param name="symbol"></param>
        /// <param name="Models"></param>
        /// <param name="SourceCompilation"></param>
        public static void AddShader(NamedTypeSymbol symbol, Dictionary<SyntaxTree, SemanticModel> Models, Compilation SourceCompilation)
        {
            if (!SymbolLookup.ContainsKey(symbol))
            {
                var shader = new ShaderClass(symbol, Models, SourceCompilation);

                SymbolLookup.Add(symbol, shader);
                Shaders.Add(shader);
            }
        }

        /// <summary>
        /// Constructor of ShaderClass. Builds dictionaries of syntax nodes and nodes which are methods from a dictionary of syntax trees and semantic models.
        /// </summary>
        /// <param name="symbol">The symbol which was the root of the shader.</param>
        /// <param name="Models">The SyntaxTree, SemanticModel dictionary for the shader.</param>
        /// <param name="SourceCompilation">The source compilation which defined the shader.</param>
        ShaderClass(NamedTypeSymbol symbol, Dictionary<SyntaxTree, SemanticModel> Models, Compilation SourceCompilation)
        {
            Symbol = symbol;
            this.Models = Models;
            this.SourceCompilation = SourceCompilation;

            // Get all syntax nodes
            Nodes = new List<SyntaxNode>();
            foreach (var node in Symbol.DeclaringSyntaxNodes)
            {
                Nodes.AddRange(node.DescendantNodes());
            }

            // Find all methods
            Methods = Nodes.OfType<MethodDeclarationSyntax>().ToList();
        }

        public NamedTypeSymbol Symbol;

        /// <summary>
        /// Nodes of the syntax tree of the shader
        /// </summary>
        public List<SyntaxNode> Nodes;
        /// <summary>
        /// Nodes of the shader's syntax tree which are methods.
        /// </summary>
        public List<MethodDeclarationSyntax> Methods;

        public ShaderClass BaseClass;

        public MethodDeclarationSyntax VertexShaderDecleration, FragmentShaderDecleration;

        public List<Dictionary<Symbol, string>> Specializations = new List<Dictionary<Symbol, string>>();

        /// <summary>
        /// Gets baseclass of shader, and vertex and fragment declerations.
        /// </summary>
        public void Setup()
        {
            GetBaseClass();
            GetVertexShaderDecleration();
            GetFragmentShaderDecleration();
        }

        /// <summary>
        /// Generates a suffix for a shader name for a specific specialization
        /// </summary>
        /// <param name="specialization"></param>
        /// <returns></returns>
        public static string SpecializationVarSuffix(Dictionary<Symbol, string> specialization)
        {
            string suffix = "";
            foreach (var variable in specialization)
            {
                suffix += string.Format("_{0}_{1}", variable.Key.Name, variable.Value.Replace("=", "_eq_").Replace(".", "p"));
            }

            return suffix;
        }

        /// <summary>
        /// Generates a filename for a specific specialized version of a shader
        /// </summary>
        /// <param name="specialization"></param>
        /// <returns></returns>
        public string SpecializationFileName(Dictionary<Symbol, string> specialization)
        {
            string suffix = "";
            foreach (var variable in specialization)
            {
                suffix += string.Format("_{0}={1}", variable.Key.Name, variable.Value);
            }

            return Symbol.Name + suffix;
        }

        /// <summary>
        /// Writes "Content.Load" commands for all specializations to BoilerWriter.
        /// </summary>
        /// <param name="BoilerWriter"></param>
        /// <param name="Tab"></param>
        public void WriteLoadCode(StringWriter BoilerWriter, string Tab)
        {
            if (!IsValidShader()) return;

            foreach (var specialization in Specializations)
            {
                string filename = SpecializationFileName(specialization);
                string suffix = SpecializationVarSuffix(specialization);
                BoilerWriter.WriteLine("{0}{0}{0}{1}.{2}.CompiledEffect{3} = Content.Load<Effect>(\"FragSharpShaders/{4}\");", Tab, Symbol.ContainingNamespace, Symbol.Name, suffix, filename);
            }
        }

        /// <summary>
        /// Compiles all specializations and writes their shader files. Adds the boilerplate from the compilization to the BoilerWriter.
        /// </summary>
        /// <param name="BoilerWriter"></param>
        /// <param name="CompileDir"></param>
        public void CompileAndWrite(StringWriter BoilerWriter, string CompileDir)
        {
            if (!IsValidShader()) return;

            foreach (var specialization in Specializations)
            {
                string name = SpecializationFileName(specialization);

                var compiled = Compile(specialization, specialization == Specializations.Last());

                // TODO: the '.fx' file ending is a HLSL thing. I don't know if this will need to change for GLSL
                TargetFile = Path.Combine(CompileDir, name) + ".fx";

                File.WriteAllText(TargetFile, compiled.Code);

                BoilerWriter.Write(compiled.Boilerplate);
                BoilerWriter.WriteLine();
            }
        }
        
        /// <summary>
        /// Creates a new ShaderWriter, compiles it. Returns a new ShaderCompilaion which has a non-null boilerplate if CompileBoilerplate is true.
        /// </summary>
        /// <param name="specialization"></param>
        /// <param name="CompileBoilerplate"></param>
        /// <returns></returns>
        public ShaderCompilation Compile(Dictionary<Symbol, string> specialization, bool CompileBoilerplate)
        {
            if (!IsValidShader()) return null;

            if (Options.ShaderLanguage == "Hlsl") {
                HlslShaderWriter writer = new HlslShaderWriter(Models, SourceCompilation, specialization);

                string fragment = writer.CompileShader(Symbol, VertexShaderDecleration, FragmentShaderDecleration);

                string boilerplate = null;
                if (CompileBoilerplate)
                {
                    boilerplate = writer.CompileShaderBoilerplate(Symbol, Specializations);
                }

                return new ShaderCompilation(fragment, boilerplate);
            }
            //else if (Options.ShaderLanguage == "Glsl")  // This is comented out because we have to be sure that Compile returns <emph> something </emph>
            else
            {
                GlslShaderWriter writer = new GlslShaderWriter(Models, SourceCompilation, specialization);

                string fragment = writer.CompileShader(Symbol, VertexShaderDecleration, FragmentShaderDecleration);

                string boilerplate = null;
                if (CompileBoilerplate)
                {
                    boilerplate = writer.CompileShaderBoilerplate(Symbol, Specializations);
                }

                return new ShaderCompilation(fragment, boilerplate);
            }
        }

        /// <summary>
        /// Checks that shader has non-null vertex and fragment shader declerations.
        /// </summary>
        /// <returns></returns>
        private bool IsValidShader()
        {
            return VertexShaderDecleration != null && FragmentShaderDecleration != null;
        }

        ShaderClass GetBaseClass()
        {
            if (BaseClass != null) return BaseClass;

            if (SymbolLookup.ContainsKey(Symbol.BaseType))
            {
                BaseClass = SymbolLookup[Symbol.BaseType];
                return BaseClass;
            }

            return null;
        }

        MethodDeclarationSyntax GetVertexShaderDecleration()
        {
            if (VertexShaderDecleration != null)
            {
                return VertexShaderDecleration;
            }

            var vertex_shaders = Methods.Where(method => HasAttribute(method, "VertexShader")).ToList();

            // If there are any (possibly multiple) vertex shaders declared, take the first one
            if (vertex_shaders.Count > 0)
            {
                VertexShaderDecleration = vertex_shaders[0];
                return VertexShaderDecleration;
            }

            // If there isn't a vertex shader defined, check the base class
            if (GetBaseClass() != null)
            {
                VertexShaderDecleration = BaseClass.GetVertexShaderDecleration();
                return VertexShaderDecleration;
            }

            // Otherwise, just give up and set it to null
            VertexShaderDecleration = null;
            return null;
        }

        MethodDeclarationSyntax GetFragmentShaderDecleration()
        {
            if (FragmentShaderDecleration != null)
            {
                return FragmentShaderDecleration;
            }

            var Fragment_shaders = Methods.Where(method => HasAttribute(method, "FragmentShader")).ToList();

            if (Fragment_shaders.Count > 0)
            {
                FragmentShaderDecleration = Fragment_shaders[0];
                return FragmentShaderDecleration;
            }

            if (BaseClass != null)
            {
                FragmentShaderDecleration = BaseClass.GetFragmentShaderDecleration();
                return FragmentShaderDecleration;
            }

            FragmentShaderDecleration = null;
            return null;
        }

        static bool HasAttribute(MethodDeclarationSyntax method, string AttributeName)
        {
            return method.AttributeLists.Any(
              list => list.Attributes.Any(
                attribute => attribute.Name.ToString() == AttributeName));
        }

        /// <summary>
        /// Gets fragment arguments which are specializations so that the specialization arguments can be factored out to increase run speed
        /// </summary>
        public void GetSpecializations()
        {
            if (!IsValidShader()) return;

            var ParameterList = FragmentShaderDecleration.ParameterList.Parameters;
            if (ParameterList.Count == 0) return;

            Specializations.Clear();
            Specializations.Add(new Dictionary<Symbol, string>());

            var first = ParameterList.First();
            foreach (var parameter in ParameterList)
            {
                var symbol = GetSymbol(parameter, Models);

                // The first parameter must be a VertexOut and can't be specialized, so skip it
                if (parameter == first)
                {
                    continue;
                }

                List<TypedConstant> speciliazation_vals = new List<TypedConstant>();

                // Get specialization values
                var Vals = symbol.GetAttribute("Vals");
                if (Vals != null && Vals.ConstructorArguments.Count() > 0)
                {
                    var args = Vals.ConstructorArguments;

                    speciliazation_vals.AddRange(args.First().Values);
                }

                // Get second-order specialization values
                foreach (var attr in symbol.GetAttributes())
                {
                    var vals_attr = attr.AttributeClass.GetAttribute("Vals");
                    if (vals_attr != null && vals_attr.ConstructorArguments.Count() > 0)
                    {
                        var args = vals_attr.ConstructorArguments;

                        speciliazation_vals.AddRange(args.First().Values);
                    }
                }

                // Add specialization values to 
                if (speciliazation_vals.Count > 0)
                {
                    List<Dictionary<Symbol, string>> copy = new List<Dictionary<Symbol, string>>(Specializations);
                    Specializations.Clear();

                    if (copy.Count == 0)
                    {
                        copy.Add(new Dictionary<Symbol, string>());
                    }

                    foreach (var specialization in copy)
                    {
                        foreach (var arg in speciliazation_vals)
                        {
                            var _specialization = new Dictionary<Symbol, string>(specialization);

                            string val_str = arg.Value.ToString();
                            if (val_str == "True") val_str = "true";
                            else if (val_str == "False") val_str = "false";

                            _specialization.Add(symbol, val_str);

                            Specializations.Add(_specialization);
                        }
                    }
                }
            }
        }
    }

    internal static class Program
    {
        static Paths BuildPaths;

        public const string Tab = "    ";

        const string ExtensionFileName = "__ExtensionBoilerplate.cs";
        const string BoilerplateFileName = "__ShaderBoilerplate.cs";

        /// <summary>
        /// Dictionary of syntax trees from SourceCompilation
        /// </summary>
        static Dictionary<SyntaxTree, SemanticModel> Models;
        /// <summary>
        /// Compilation of source code from build path.
        /// </summary>
        static Compilation SourceCompilation;
        /// <summary>
        /// List of nodes from SourceCompilations
        /// </summary>
        static List<SyntaxNode> Nodes;

        /// <summary>
        /// Returns true is symbol is a shader or has a BaseTYpe of FragSharpFramework.FragSharpStd
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns></returns>
        static bool IsShader(this NamedTypeSymbol symbol)
        {
            if (symbol.BaseType == null) return false;
            else return 
                    symbol.BaseType.ToString() == "FragSharpFramework.FragSharpStd" ||
                    symbol.BaseType.IsShader();
        }

        /// <summary>
        /// Given a syntax node, this retreives it's semantic model from the Models property (dictionary)
        /// </summary>
        /// <param name="node"></param>
        /// <returns></returns>
        static SemanticModel Model(SyntaxNode node)
        {
            return Models[node.SyntaxTree];
        }

        // TODO: I can see in WAL that [GLSL("vec4")] attributes are ending up [GLSL("<class name>")] in __ExtensionBoilerplate.cs. seems to be a bug
        /// <summary>
        /// Writes file __ExtensionBoilerplate which contains all of the namespaces and public structures/classes and methods. 
        /// Returns true if __ExtensionBoilerplate.cs was changed.
        /// </summary>
        /// <param name="Models"></param>
        /// <param name="Nodes"></param>
        /// <returns></returns>
        static bool CreateExtensionBoilerplate(Dictionary<SyntaxTree, SemanticModel> Models, IEnumerable<SyntaxNode> Nodes)
        {
            StringWriter writer = new StringWriter();

            // TODO: The following will have to be changed when we move to MonoGame
            writer.WriteLine(
@"// This file was auto-generated by FragSharp. It will be regenerated on the next compilation.
// Manual changes made will not persist and may cause incorrect behavior between compilations.

using System;
using System.Collections.Generic;

using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Content;
using Microsoft.Xna.Framework.Graphics;

using FragSharpFramework;
");

            var classes = Nodes.OfType<TypeDeclarationSyntax>();
            HashSet<Symbol> Processed = new HashSet<Symbol>(); // Keep track of which classes have been processed so we don't double-process.

            foreach (var _class in classes)
            {
                var copy_attribute = GetCopyAttribute(_class);
                if (copy_attribute == null) continue;

                var args = copy_attribute.ArgumentList.Arguments;
                var arg = args[0].Expression as TypeOfExpressionSyntax;
                var type = Models[_class.SyntaxTree].GetSymbolInfo(arg.Type).Symbol as NamedTypeSymbol;

                string cast_type = "explicit";
                if (args.Count >= 2)
                {
                    var style_name = Models[_class.SyntaxTree].GetSymbolInfo(args[1].Expression).Symbol.Name;

                    if (style_name == Enum.GetName(typeof(CastStyle), CastStyle.ImplicitCast)) cast_type = "implicit";
                    if (style_name == Enum.GetName(typeof(CastStyle), CastStyle.ExplicitCasts)) cast_type = "explicit";
                    if (style_name == Enum.GetName(typeof(CastStyle), CastStyle.NoCasts)) cast_type = null;
                }

                var class_symbol = Models[_class.SyntaxTree].GetDeclaredSymbol(_class);
                if (Processed.Contains(class_symbol)) continue;
                Processed.Add(class_symbol);

                if (type != null)
                {
                    var code = type.DeclaringSyntaxNodes.First();

                    var output = code.ToFullString().Replace(type.Name, class_symbol.Name);
                    output = output.Replace("/*KeepInCopy*/"  + class_symbol.Name, type.Name);
                    output = output.Replace("/*KeepInCopy*/ " + class_symbol.Name, type.Name);

                    switch (cast_type)
                    {
                        case null:
                            output = output.Replace("// Extra code gen goes here", "");
                            break;

                        default:
                            output = output.Replace("// Extra code gen goes here",
                                string.Format(@"public static {2} operator {1}(vec4 v) {{ return new {1}(v.x, v.y, v.z, v.w); }}
        public static {2} operator vec4({1} v) {{ return new vec4(v.x, v.y, v.z, v.w); }}", " ", class_symbol.Name, cast_type));

                            break;
                    }
                    
                    writer.WriteLine("namespace {0}", class_symbol.ContainingNamespace);
                    writer.Write("{");
                    writer.Write(output);
                    writer.WriteLine("}");
                    writer.WriteLine();
                }
            }

            var text = writer.ToString();
            string path = Path.Combine(BuildPaths.BoilerRoot, ExtensionFileName);

            try
            {
                if (text.GetHashCode() == File.ReadAllText(path).GetHashCode())
                {
                    return false;
                }
            }
            catch
            {
            }

            Directory.CreateDirectory(BuildPaths.BoilerRoot);
            File.WriteAllText(path, writer.ToString());
            return true;
        }

        /// <summary>
        /// Gets the "Vals" attribute of a type decalaration.
        /// </summary>
        /// <param name="_class"></param>
        /// <returns></returns>
        static AttributeSyntax GetValsAttribute(TypeDeclarationSyntax _class)
        {
            return GetAttribute(_class, "Vals");
        }

        /// <summary>
        /// Gets the copy attribute of a type declaration.
        /// </summary>
        /// <param name="_class"></param>
        /// <returns></returns>
        static AttributeSyntax GetCopyAttribute(TypeDeclarationSyntax _class)
        {
            return GetAttribute(_class, "Copy");
        }

        /// <summary>
        /// Given _class, loops through child nodes. For each child node which is a non-null AttributeListSyntax, loops through attributes. Returns name of first attribute found in this way.
        /// </summary>
        /// <param name="_class"></param>
        /// <param name="AttributeName"></param>
        /// <returns></returns>
        static AttributeSyntax GetAttribute(TypeDeclarationSyntax _class, string AttributeName)
        {
            foreach (var node in _class.ChildNodes())
            {
                var attribute_node = node as AttributeListSyntax;
                if (null != attribute_node)
                {
                    foreach (var attribute in attribute_node.Attributes)
                    {
                        if (attribute.Name.ToString() == AttributeName)
                        {
                            return attribute;
                        }
                    }
                }
            }

            return null;
        }

        /// <summary>
        /// Parses command line arguments. If none are provided, defaults to hard coded values for source fir, output dir, and configuration
        /// </summary>
        /// <param name="args"></param>
        static void ParseArgs(string[] args)
        {
            if (args.Length < 3)
            {
                Console.WriteLine("FragSharp requires three arguments: Source directory, Output directory, Configuration.");
                Console.WriteLine("Defaulting to debug directories.");

                //ParseArgs(new string[] {
                //    /* Source */        "C:/Users/Jordan/Desktop/Dir/Projects/FragSharp/Examples/Life/",
                //    /* Output */        "C:/Users/Jordan/Desktop/Dir/Projects/FragSharp/Examples/Life/bin/x86/Release/",
                //    /* Configuration */ "Debug"
                //});

                //ParseArgs(new string[] {
                //    /* Source */        "C:/Users/John Novak/Documents/GitHub/FragSharp/Examples/Life/",
                //    /* Output */        "C:/Users/John Novak/Documents/GitHub/FragSharp/Examples/Life/bin/x86/Release/",
                //    /* Configuration */ "Debug",
                //});

                ParseArgs(new string[] {
                    /* Source */        "C:/Users/John Novak/Documents/GitHub/We-Are-Legion/Game/Game.csproj",
                    /* Output */        "C:/Users/John Novak/Documents/GitHub/We-Are-Legion/Game/bin/x86/Debug/",
                    /* Configuration */ "Debug",
                });

                //ParseArgs(new string[] {
                //    /* Source */        "C:/Users/Jordan/Desktop/Dir/Pwnee/Games/Terracotta/Terracotta/Terracotta/Terracotta/Terracotta.csproj",
                //    /* Output */        "C:/Users/Jordan/Desktop/Dir/Pwnee/Games/Terracotta/Terracotta/Terracotta/Terracotta/bin/x86/Release/",
                //    /* Configuration */ "Release"
                //});
            }
            else
            {
                BuildPaths = new Paths(args);
            }
        }

        private static void Main(string[] args)
        {
            try
            {
                _Main(args);
            }
            catch (Exception e)
            {
                Console.Write(e.ToString());
                Console.ReadLine();
                File.WriteAllText("dump.report", e.ToString());
            }
        }

        private static void _Main(string[] args)
        {
            ParseArgs(args);

            // Get and compile the user's code
            CompileUserCode();

            // Create __ExtensionBoilerplate.cs
            bool changed = CreateExtensionBoilerplate(Models, Nodes);
            
            // Recompile
            if (changed)
                CompileUserCode();

            // Create translation lookup
            TranslationLookup.ProcessTypes(Models, Nodes);
            TranslationLookup.ProcessMembers(Models, Nodes);
            TranslationLookup.ProcessReadonlys(Models, Nodes, SourceCompilation);

            // Find all shader classes
            var classes = Nodes.OfType<ClassDeclarationSyntax>();

            foreach (var _class in classes)
            {
                var symbol = Model(_class).GetDeclaredSymbol(_class);

                if (IsShader(symbol))
                    ShaderClass.AddShader(symbol, Models, SourceCompilation);
            }

            foreach (var shader in ShaderClass.Shaders)
            {
                shader.Setup();
                Console.WriteLine("{0}, vertex = {1}, fragment = {2}", shader.Symbol,
                    shader.VertexShaderDecleration == null ? "none" : shader.VertexShaderDecleration.Identifier.ToString(),
                    shader.FragmentShaderDecleration == null ? "none" : shader.FragmentShaderDecleration.Identifier.ToString());
            }

            foreach (var shader in ShaderClass.Shaders)
            {
                shader.GetSpecializations();
            }

            // Create shader directory to store compiled shaders. Empty it if it has files in it.
            Directory.CreateDirectory(BuildPaths.ShaderCompileDir);
            foreach (var file in Directory.GetFiles(BuildPaths.ShaderCompileDir, "*", SearchOption.AllDirectories))
            {
                File.Delete(file);
            }

            // Compile shaders from C# to target language
            StringWriter BoilerWriter = new StringWriter();
            /*switch (Options.ShaderLanguage)
            {
                case "Hlsl":
                    BoilerWriter.WriteLine(HlslShaderWriter.BoilerFileBegin, Tab);
                    BoilerWriter.WriteLine();

                    BoilerWriter.WriteLine(HlslShaderWriter.BoilerBeginInitializer, Tab);

                    foreach (var shader in ShaderClass.Shaders)
                    {
                        shader.WriteLoadCode(BoilerWriter, Tab);
                    }

                    BoilerWriter.WriteLine(HlslShaderWriter.BoilerEndInitializer, Tab);
                    BoilerWriter.WriteLine();
                    break;
                case "Glsl":
                    BoilerWriter.WriteLine(GlslShaderWriter.BoilerFileBegin, Tab);
                    BoilerWriter.WriteLine();

                    BoilerWriter.WriteLine(GlslShaderWriter.BoilerBeginInitializer, Tab);

                    foreach (var shader in ShaderClass.Shaders)
                    {
                        shader.WriteLoadCode(BoilerWriter, Tab);
                    }

                    BoilerWriter.WriteLine(GlslShaderWriter.BoilerEndInitializer, Tab);
                    BoilerWriter.WriteLine();
                    break;
                default:
                    Console.WriteLine("Invalid Shader Langauge Selected: {0}", Options.ShaderLanguage);
                    break;
            }*/
            BoilerWriter.WriteLine(HlslShaderWriter.BoilerFileBegin, Tab);
            BoilerWriter.WriteLine();

            BoilerWriter.WriteLine(HlslShaderWriter.BoilerBeginInitializer, Tab);

            foreach (var shader in ShaderClass.Shaders)
            {
                shader.WriteLoadCode(BoilerWriter, Tab);
            }

            BoilerWriter.WriteLine(HlslShaderWriter.BoilerEndInitializer, Tab);
            BoilerWriter.WriteLine();

            foreach (var shader in ShaderClass.Shaders)
            {
                shader.CompileAndWrite(BoilerWriter, BuildPaths.ShaderCompileDir);
            }

            Directory.CreateDirectory(BuildPaths.BoilerRoot);
            File.WriteAllText(Path.Combine(BuildPaths.BoilerRoot, BoilerplateFileName), BoilerWriter.ToString());

            // Compile target shaders
            BuildGeneratedShaders();
            //BuildGeneratedShaders2();
            Console.ReadLine();
        }

        static void BuildGeneratedShaders2()
        {
            // Empty the build directory
            Directory.CreateDirectory(BuildPaths.ShaderBuildDir);
            foreach (var file in Directory.GetFiles(BuildPaths.ShaderBuildDir))
            {
                File.Delete(file);
            }

            // Build each shader
            foreach (var file in Directory.GetFiles(BuildPaths.ShaderCompileDir, "*.fx"))
            {
                string fx = File.ReadAllText(file);

                EffectProcessor effectProcessor = new EffectProcessor();
                //effectProcessor.DebugMode = EffectProcessorDebugMode.Debug;
                effectProcessor.DebugMode = EffectProcessorDebugMode.Optimize;
                var effect = effectProcessor.Process(new EffectContent { EffectCode = fx }, new MyProcessorContext());

                byte[] ShaderObject = effect.GetEffectCode();

                string output_file = Path.Combine(BuildPaths.ShaderBuildDir, Path.GetFileNameWithoutExtension(file)) + ".xnb";
                File.WriteAllBytes(output_file, ShaderObject);
            }

            //foreach (var shader in ShaderClass.Shaders)
            //{
            //    if (shader.TargetFile == null) continue;

            //    string fx = File.ReadAllText(shader.TargetFile);

            //    EffectProcessor effectProcessor = new EffectProcessor();
            //    effectProcessor.DebugMode = EffectProcessorDebugMode.Debug;
            //    var effect = effectProcessor.Process(new EffectContent { EffectCode = fx }, new MyProcessorContext());

            //    byte[] ShaderObject = effect.GetEffectCode();

            //    string output_file = Path.Combine(BuildPaths.ShaderBuildDir, Path.GetFileNameWithoutExtension(shader.TargetFile)) + ".xnb";
            //    File.WriteAllBytes(output_file, ShaderObject);
            //}
        }

        class MyProcessorLogger : ContentBuildLogger
        {
            public static string Log = string.Empty;
            public override void LogMessage(string message, params object[] messageArgs) { Log += message; }
            public override void LogImportantMessage(string message, params object[] messageArgs) { Log += message; }
            public override void LogWarning(string helpLink, ContentIdentity contentIdentity, string message, params object[] messageArgs) { Log += message; }
        }

        class MyProcessorContext : ContentProcessorContext
        {
            public override TargetPlatform TargetPlatform { get { return TargetPlatform.Windows; } }
            public override GraphicsProfile TargetProfile { get { return GraphicsProfile.HiDef; } }
            public override string BuildConfiguration { get { return string.Empty; } }
            public override string IntermediateDirectory { get { return string.Empty; } }
            public override string OutputDirectory { get { return string.Empty; } }
            public override string OutputFilename { get { return string.Empty; } }

            public override OpaqueDataDictionary Parameters { get { return parameters; } }
            OpaqueDataDictionary parameters = new OpaqueDataDictionary();

            public override ContentBuildLogger Logger { get { return logger; } }
            ContentBuildLogger logger = new MyProcessorLogger();

            public override void AddDependency(string filename) { }
            public override void AddOutputFile(string filename) { }

            public override TOutput Convert<TInput, TOutput>(TInput input, string processorName, OpaqueDataDictionary processorParameters) { throw new NotImplementedException(); }
            public override TOutput BuildAndLoadAsset<TInput, TOutput>(ExternalReference<TInput> sourceAsset, string processorName, OpaqueDataDictionary processorParameters, string importerName) { throw new NotImplementedException(); }
            public override ExternalReference<TOutput> BuildAsset<TInput, TOutput>(ExternalReference<TInput> sourceAsset, string processorName, OpaqueDataDictionary processorParameters, string importerName, string assetName) { throw new NotImplementedException(); }
        }

        static void BuildGeneratedShaders()
        {
            ContentBuilder contentBuilder = new ContentBuilder();

            contentBuilder.Clear();

            foreach (var file in Directory.GetFiles(BuildPaths.ShaderCompileDir, "*.fx"))
            {
                string name = Path.GetFileNameWithoutExtension(file);

                contentBuilder.Add(file, name, "EffectImporter", "EffectProcessor");
            }

            //foreach (var shader in ShaderClass.Shaders)
            //{
            //    if (shader.TargetFile == null) continue;

            //    contentBuilder.Add(shader.TargetFile, shader.Symbol.Name, "EffectImporter", "EffectProcessor");
            //}

            // Empty the build directory
            Directory.CreateDirectory(BuildPaths.ShaderBuildDir);
            foreach (var file in Directory.GetFiles(BuildPaths.ShaderBuildDir))
            {
                File.Delete(file);
            }

            // Build the shaders
            string buildError = contentBuilder.Build();

            if (buildError != null)
            {
                Console.WriteLine(buildError);
                return;
            }

            /* fxc build, debug, and asm output
            if (buildError != null && buildError.Length > 0)
            {
                string fxc = "C:/Program Files (x86)/Microsoft DirectX SDK (June 2010)/Utilities/bin/x86/fxc.exe";
                string file = "C:/Users/Jordan/Desktop/Dir/Projects/Million/GpuSim/GpuSim/GpuSim/__GeneratedShaders/_Counting.fx";
                
                //string arguments = string.Format("/Od /Zi /Tfx_2_0 /Fo {0}o {0}", file);
                //string arguments = string.Format("/Tfx_2_0 /Fc {0}o.asm {0}", file); // Generate asm text
                string arguments = string.Format("/Tfx_2_0 /Fe {0}.out {0}", file); // Generate errors and warnings

                string output = RunCommand(fxc, arguments);
                Console.WriteLine(output);
            }
            */

            var files = Directory.GetFiles(contentBuilder.BuiltDirectory);

            foreach (var file in files)
            {
                string new_file = Path.Combine(BuildPaths.ShaderBuildDir, Path.GetFileName(file));
                File.Copy(file, new_file);
            }
        }

        /// <summary>
        /// Loads project specified in build paths, parses with Roslyn, and sets Nodes and Models properties, corresponding to the nodes of the syntax trees and syntax trees respectively.
        /// </summary>
        static void CompileUserCode()
        {
            //// Get all the relevant source files
            //var files =    Directory.GetFiles(BuildPaths.ProjectDir, "*.cs", SearchOption.AllDirectories).ToList();
            //files.AddRange(Directory.GetFiles(BuildPaths.FrameworkDir, "*.cs", SearchOption.AllDirectories).ToList());

            //// Get all the syntax trees from the source files
            //List<SyntaxTree> Trees = new List<SyntaxTree>();
            //foreach (var file in files)
            //{
            //    Trees.Add(SyntaxTree.ParseFile(file));
            //}

            //Nodes = new List<SyntaxNode>();
            //foreach (var tree in Trees)
            //{
            //    Nodes.AddRange(tree.GetRoot().DescendantNodes());
            //}

            //// Compile all the sources together
            //SourceCompilation = Compilation.Create("MyCompilation",
            //                                 syntaxTrees: Trees,
            //                                 references: new List<MetadataReference>() { MetadataReference.CreateAssemblyReference(typeof(object).Assembly.FullName) });

            DocumentId id;
            //var proj = Solution.LoadStandAloneProject(BuildPaths.ProjectPath, configuration: BuildPaths.Configuration);
            var proj = Solution.LoadStandAloneProject("C:/Users/John Novak/Documents/GitHub/We-Are-Legion/Game/Game.csproj", "Debug");
            //var proj = Solution.LoadStandAloneProject("C:/Users/John Novak/Documents/GitHub/FragSharp/Examples/Life/Life Example.csproj", "Debug");
            //var proj = Solution.LoadStandAloneProject(BuildPaths.ProjectPath, configuration: "Release");
            //var proj = Solution.LoadStandAloneProject(BuildPaths.ProjectPath);
            foreach (var pre in proj.ParseOptions.PreprocessorSymbolNames)
                Console.WriteLine(pre);
            foreach (var file in Directory.GetFiles(BuildPaths.FrameworkDir, "*.cs", SearchOption.AllDirectories))
                proj = proj.AddDocument(file, out id);
            SourceCompilation = (Compilation)proj.GetCompilation();

            Nodes = new List<SyntaxNode>();
            foreach (var tree in SourceCompilation.SyntaxTrees)
            {
                Nodes.AddRange(tree.GetRoot().DescendantNodes());
            }

            Models = new Dictionary<SyntaxTree, SemanticModel>();
            foreach (var tree in SourceCompilation.SyntaxTrees)
            {
                var model = SourceCompilation.GetSemanticModel(tree);
                Models.Add(tree, model);
            }
        }

        /// <summary>
        /// Prints the syntax tree (from Roslyn) to the console for a given node
        /// </summary>
        /// <param name="node"></param>
        /// <param name="indent"></param>
        static void PrintTree(SyntaxNodeOrToken node, string indent = "")
        {
            using (var writer = new StringWriter())
            {
                WriteTree(node, writer, indent);
                Console.Write(writer);
            }
        }

        /// <summary>
        /// Given a syntax node or token and an output, generates a text representation of the syntax tree and writes it to the output.
        /// </summary>
        /// <param name="node"></param>
        /// <param name="output"></param>
        /// <param name="indent"></param>
        static void WriteTree(SyntaxNodeOrToken node, StringWriter output, string indent = "")
        {
            var nodes = node.ChildNodesAndTokens();
            foreach (var child in nodes)
            {
                string kind = string.Empty;
                string value = string.Empty;

                if (child.IsNode)
                {
                    kind = child.AsNode().Kind.ToString();
                }
                else
                {
                    kind = child.AsToken().Kind.ToString();
                    value = child.AsToken().ValueText;
                }

                output.WriteLine("{0}{1}  {2}", indent, kind, value);

                WriteTree(child, output, indent + "--");
            }
        }

        static bool IsGridComputtion(MethodDeclarationSyntax method)
        {
            return true;
        }
        
        /// <summary>
        /// Recusively gets all nodes in a syntax tree
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="node"></param>
        /// <param name="methods"></param>
        /// <returns></returns>
        static List<T> GetNodes<T>(SyntaxNodeOrToken node, List<T> methods = null) where T : class
        {
            if (methods == null)
            {
                methods = new List<T>();
            }

            var nodes = node.ChildNodesAndTokens();
            foreach (var child in nodes)
            {
                var method = child.AsNode() as T;
                if (null != method)
                {
                    methods.Add(method);
                }

                GetNodes(child, methods);
            }

            return methods;
        }

        static string RunCommand(string Executable, string Arguments)
        {
            // Start the child process.
            Process p = new Process();

            // Redirect the output stream of the child process.
            p.StartInfo.UseShellExecute = false;
            p.StartInfo.RedirectStandardOutput = true;
            p.StartInfo.FileName = Executable;
            p.StartInfo.Arguments = Arguments;

            p.Start();

            string output = p.StandardOutput.ReadToEnd();
            p.WaitForExit();

            return output;
        }
   }
}
