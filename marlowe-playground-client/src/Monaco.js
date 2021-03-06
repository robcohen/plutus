/*eslint-env node*/
'use strict';

exports.isWarning_ = function (severity) {
  return severity == 4;
}

exports.isError_ = function (severity) {
  return severity == 8;
}

exports.getMonaco = function () {
  return global.monaco;
}

exports.registerLanguage_ = function (monaco, language) {
  monaco.languages.register(language);
}

exports.defineTheme_ = function (monaco, theme) {
  monaco.editor.defineTheme(theme.name, theme.themeData);
}

exports.setMonarchTokensProvider_ = function (monaco, languageId, languageDef) {
  monaco.languages.setMonarchTokensProvider(languageId, languageDef);
}

exports.setModelMarkers_ = function (monaco, model, owner, markers) {
  monaco.editor.setModelMarkers(model, owner, markers);
}

exports.getModelMarkers_ = function (monaco, model) {
  return monaco.editor.getModelMarkers({ resource: model.uri });
}

exports.create_ = function (monaco, nodeId, languageId) {
  const editor = monaco.editor.create(nodeId, {
    value: [
      'Close'
    ].join('\n'),
    language: languageId,
  });
  return editor;
}

exports.setTheme_ = function (monaco, themeName) {
  monaco.editor.setTheme(themeName);
}

exports.onDidChangeContent_ = function (editor, handler) {
  editor.getModel().onDidChangeContent(function (event) {
    handler(event)();
  });
}

exports.getModel_ = function (editor) {
  return editor.getModel();
}

exports.getEditorId_ = function(editor) {
  return editor.getId();
}

exports.getValue_ = function (model) {
  return model.getValue();
}

exports.setValue_ = function (model, value) {
  return model.setValue(value);
}

exports.setTokensProvider_ = function (monaco, languageId, provider) {
  monaco.languages.setTokensProvider(languageId, provider);
}

exports.completionItemKind_ = function (name) {
  return monaco.languages.CompletionItemKind[name];
}

exports.markerSeverity_ = function (name) {
  return monaco.MarkerSeverity[name];
}

exports.registerHoverProvider_ = function (monaco, languageId, provider) {
  monaco.languages.registerHoverProvider(languageId, provider);
}

exports.registerCompletionItemProvider_ = function (monaco, languageId, provider) {
  monaco.languages.registerCompletionItemProvider(languageId, provider);
}

exports.registerCodeActionProvider_ = function (monaco, languageId, actionProvider) {
  monaco.languages.registerCodeActionProvider(languageId, actionProvider);
}

exports.registerDocumentFormattingEditProvider_ = function (monaco, languageId, formatter) {
  monaco.languages.registerDocumentFormattingEditProvider(languageId, formatter);
}

exports.setPosition_ = function (editor, position) {
  editor.setPosition(position);
}

exports.revealLine_ = function (editor, lineNumber) {
  editor.revealLine(lineNumber);
}

exports.layout_ = function (editor) {
  editor.layout();
}

exports.enableVimBindings_ = function (editor) {
  var vimMode = global.initVimMode(editor);
  return (() => vimMode.dispose());
}

exports.enableEmacsBindings_ = function (editor) {
  var emacsMode = new global.EmacsExtension(editor);
  emacsMode.start();
  return (() => emacsMode.dispose());
}