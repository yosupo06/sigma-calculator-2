import React, { useState } from "react";
import Editor from "@monaco-editor/react";

interface Props {
  value: string;
  language: "txt" | "cpp";
  onChange?: (value: string) => void;
  readOnly: boolean;
  autoHeight: boolean;
}

const editorMode = (lang: "txt" | "cpp") => {
  if (lang == "txt") {
    return "plaintext";
  }
  if (lang == "cpp") {
    return "cpp";
  }
};

const SourceEditor: React.FC<Props> = (props) => {
  const minHeight = 100;
  const { value, language, onChange, readOnly, autoHeight } = props;
  const [editorHeight, setEditorHeight] = useState(minHeight);

  const mode = editorMode(language);

  return (
    <Editor
      value={value}
      language={mode}
      height={autoHeight ? undefined: editorHeight}
      onChange={(src) => {
        if (src !== undefined && onChange) onChange(src);
      }}
      onMount={(editor) => {
        editor.onDidContentSizeChange(() => {
          if (autoHeight) {
            setEditorHeight(Math.max(minHeight, editor.getContentHeight()));
          }
        });
      }}
      options={{
        readOnly: readOnly,
        scrollBeyondLastColumn: 0,
        scrollBeyondLastLine: false,
        minimap: {
          enabled: false,
        },
        scrollbar: {
          alwaysConsumeMouseWheel: false,
        },
      }}
    />
  );
};

export default SourceEditor;
