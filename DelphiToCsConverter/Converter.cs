using Microsoft.CSharp;
using System;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DGrok.Framework;
using DGrok.DelphiNodes;

namespace DelphiToCsConverter
{
    public class Converter
    {
        public string Convert(string delphiCode)
        {
            if (delphiCode.Contains("class"))
            {
#if NEVER
                // Parse without filtering out comment tokens...
                Lexer lexer = new Lexer(delphiCode, "input");
                Parser parser = Parser.FromTokens(lexer.Tokens);
#endif

                Parser parser = Parser.FromText(delphiCode, "input", CompilerDefines.CreateStandard(),
                    new MemoryFileLoader());

                AstNode tree = parser.ParseRule(RuleType.Unit);
                //var Text = tree.Inspect();

                UnitNode unitNode = (UnitNode)tree;

                CodeCompileUnit compileUnit = new CodeCompileUnit();
                CodeNamespace defaultNamespace = new CodeNamespace(unitNode.UnitNameNode.Text);
                //defaultNamespace.Imports.Add(new CodeNamespaceImport("System"));

                compileUnit.Namespaces.Add(defaultNamespace);

                foreach (TypeSectionNode typeSectionNode in unitNode.InterfaceSectionNode.ContentListNode.Items.Where(o => o is TypeSectionNode))
                {
                    foreach (TypeForwardDeclarationNode node in typeSectionNode.TypeListNode.Items)
                    {
                        if (node.TypeNode.Text == "class")
                        {
                            CodeTypeDeclaration nameClass = new CodeTypeDeclaration(node.NameNode.Text);
                            defaultNamespace.Types.Add(nameClass);
                        }
                    }
                }

                using (StringWriter sw = new StringWriter())
                {
                    IndentedTextWriter tw = new IndentedTextWriter(sw, "    ");
                    CSharpCodeProvider provider = new CSharpCodeProvider();

                    provider.GenerateCodeFromCompileUnit(
                        compileUnit,
                        tw,
                        GetCodeGenerationOptions());

                    tw.Close();

                    return sw.ToString();
                }
            }
            else
            {
                return @"
class TForm1
{
    void Button1Click(Sender: TObject)
    {
    }
};
";
            }
        }

        private static CodeGeneratorOptions GetCodeGenerationOptions()
        {
            var options = new CodeGeneratorOptions();
            options.BracingStyle = "C";
            return options;
        }
    }
}
