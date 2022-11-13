import React, { useState } from "react";
import Editor from "@monaco-editor/react";

interface Props {
  value: string;
  language: "txt" | "cpp";
  onChange?: (value: string) => void;
  readOnly: boolean;
  height: number;
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
  const { value, language, onChange, readOnly, height } = props;

  const mode = editorMode(language);

  return (
    <Editor
      value={value}
      language={mode}
      height={height}
      onChange={(src) => {
        if (src !== undefined && onChange) onChange(src);
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
