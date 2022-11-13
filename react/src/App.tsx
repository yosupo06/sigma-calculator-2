import { Box, Button, CssBaseline, Paper, Typography } from "@mui/material";
import NavBar from "./components/NavBar";
import React, { useState } from "react";
import { to_cpp_code } from "../sigma";
import "./App.css"
import SourceEditor from "./components/SourceEditor";
import { Container, textAlign } from "@mui/system";

const App = () => {
  const [input, setInput] = useState("");
  const [output, setOutput] = useState("");

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    setOutput(to_cpp_code(input))
  };

  return (
    <React.Fragment>
      <CssBaseline />
      <Box>
        <NavBar />
      </Box>
      <Box>
        <Container>
          <Box sx={{ margin: 1 }}>
            <Paper>
              <Typography variant="h4">Input</Typography>
              <SourceEditor readOnly={false} value={input} onChange={setInput} autoHeight={false} language="txt"></SourceEditor>
            </Paper>
          </Box>
          <Box sx={{ textAlign: "center"}}>
            <Button variant="contained" onClick={handleSubmit}>Convert</Button>
          </Box>

          <Box sx={{ margin: 1 }}>
            <Paper>
              <Typography variant="h4">Output</Typography>
              <SourceEditor readOnly={true} value={output} autoHeight={false} language="txt"></SourceEditor>
            </Paper>
          </Box>
        </Container>
      </Box>
    </React.Fragment>
  );
}

export default App;
