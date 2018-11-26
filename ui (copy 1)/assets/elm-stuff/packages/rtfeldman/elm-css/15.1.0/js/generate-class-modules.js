// @flow

/*::
type ModuleDeclaration = {
  name: string,
  values: Array<{ name : string, signature : string }>
}
*/

const path = require("path"),
  mkdirp = require("mkdirp"),
  fs = require("fs-extra");

function classNameForValue(moduleName /*: string*/, valueName /*: string*/) {
  return (
    moduleName.replace(allDots, "-") + "-" + valueName.replace(allDots, "-")
  );
}

const allDots = /\./g;

function declarationForValue(
  moduleName /*: string*/,
  valueName /*: string */,
  isSvg /*: boolean */
) {
  const functionCall = isSvg
    ? 'Html.attribute "class"'
    : "Html.Attributes.class";

  return (
    valueName +
    " : Html.Attribute msg\n" +
    valueName +
    " =\n    " +
    functionCall +
    ' "' +
    classNameForValue(moduleName, valueName) +
    '"'
  );
}

function isUniqueSvgClass(signature) {
  return signature === "DEPRECATED.Css.File.UniqueSvgClass";
}

function declarationsForModule(modul /*: ModuleDeclaration */) {
  return modul.values
    .filter(function(value) {
      return (
        value.signature === "DEPRECATED.Css.File.UniqueClass" ||
        isUniqueSvgClass(value.signature)
      );
    })
    .map(function(value) {
      return declarationForValue(
        modul.name,
        value.name,
        isUniqueSvgClass(value.signature)
      );
    });
}

function moduleHeader(moduleName /*: string */) {
  return (
    "module " +
    moduleName +
    ".Styles exposing (..)\n\n" +
    "{-| This file was generated by the elm-css binary. Don't edit it, because it will be overwritten!\n-}\n\n" +
    "import Html\nimport Html.Attributes\n\n\n"
  );
}

function generateModule(modul /*: ModuleDeclaration */) {
  // final newline is for posix compliance
  return (
    moduleHeader(modul.name) +
    declarationsForModule(modul).join("\n\n\n") +
    "\n"
  );
}

function writeFile(
  generatedCodeRoot /*: string*/,
  modul /*: ModuleDeclaration*/
) {
  const directory = path.join.apply(
    path,
    [generatedCodeRoot].concat(modul.name.split("."))
  );
  return new Promise(function(resolve, reject) {
    mkdirp(directory, function(dirError) {
      if (dirError) return reject(dirError);

      const filename = path.join(directory, "Styles.elm");
      fs.writeFile(filename, generateModule(modul), function(fileError) {
        if (fileError) return reject(fileError);

        resolve(filename);
      });
    });
  });
}

module.exports = {
  classNameForValue: classNameForValue,
  writeFile: writeFile
};
