import { Accordion, AccordionDetails, AccordionSummary, Box, Button, CssBaseline, Paper, Table, TableBody, TableCell, TableContainer, TableHead, TableRow, Typography } from "@mui/material";
import NavBar from "./components/NavBar";
import React, { useState } from "react";
import { to_cpp_code } from "../sigma";
import "./App.css"
import { ExpandMore } from "@mui/icons-material";
import SourceEditor from "./components/SourceEditor";
import { Container, textAlign } from "@mui/system";

const App = () => {
  const [input, setInput] = useState("");
  const [output, setOutput] = useState("");

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    setOutput(to_cpp_code(input))
  };

  const samples = [
    {name: "ABC269 F", data: "f(M, A, B, C, D)=$(i=A..B)$(j=C..D)[2|i+j]((i-1)*M+j)"}
  ]
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
              <SourceEditor readOnly={false} value={input} onChange={setInput} height={100} language="txt"></SourceEditor>
            </Paper>
          </Box>
          <Box sx={{ margin: 1 }}>
            <Accordion>
              <AccordionSummary expandIcon={<ExpandMore />}>
                <Typography>Examples:</Typography>
              </AccordionSummary>
              <AccordionDetails>
                <TableContainer>
                  <Table>
                    <TableHead>
                      <TableRow>
                        <TableCell>Problem</TableCell>
                        <TableCell>Code</TableCell>
                      </TableRow>
                    </TableHead>
                    <TableBody>
                      {samples.map((row) => (
                        <TableRow key={row.name}>
                          <TableCell>{row.name}</TableCell>
                          <TableCell>{row.data}</TableCell>
                        </TableRow>
                      ))}
                    </TableBody>
                  </Table>
                </TableContainer>
              </AccordionDetails>
            </Accordion>
          </Box>          
          <Box sx={{ textAlign: "center"}}>
            <Button variant="contained" onClick={handleSubmit}>Convert</Button>
          </Box>

          <Box sx={{ margin: 1 }}>
            <Paper>
              <Typography variant="h4">Output</Typography>
              <SourceEditor readOnly={true} value={output} height={300} language="txt"></SourceEditor>
            </Paper>
          </Box>
        </Container>
      </Box>
    </React.Fragment>
  );
}

export default App;
