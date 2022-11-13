import Button from "@mui/material/Button";
import AppBar from "@mui/material/AppBar";
import Typography from "@mui/material/Typography";
import React from "react";
import Box from "@mui/material/Box";
import logo from './icon.png';
import { Avatar, Toolbar } from "@mui/material";

const NavBar = () => {
  return (
    <Box sx={{ flexGlow: 1}}>
      <AppBar position="static">
        <Toolbar>
          <Avatar src={logo} sx={{height: 64, width:64}}/>
          <Typography variant="h6" noWrap>Sigma Calculator</Typography>
        </Toolbar>
      </AppBar>
    </Box>
  );
};

export default NavBar;
