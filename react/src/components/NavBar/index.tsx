import Button from "@mui/material/Button";
import AppBar from "@mui/material/AppBar";
import Typography from "@mui/material/Typography";
import React from "react";
import Box from "@mui/material/Box";
import logo from './icon.png';
import { Avatar, Toolbar } from "@mui/material";
import { GitHub } from "@mui/icons-material";

const NavBar = () => {
  return (
    <Box sx={{ flexGlow: 1}}>
      <AppBar position="static">
        <Toolbar>
          <Avatar src={logo} sx={{height: 64, width:64}}/>
          <Typography variant="h6" noWrap>Sigma Calculator (Beta)</Typography>
          <Button
            color="inherit"
            href="https://github.com/yosupo06/sigma-calculator-2"
            target="_blank"
            rel="noopener"
            sx={{
              ml: "auto",
              mr: 0.2,
            }}

          >
            <GitHub />
          </Button>
        </Toolbar>
      </AppBar>
    </Box>
  );
};

export default NavBar;
