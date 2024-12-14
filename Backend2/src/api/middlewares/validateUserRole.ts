import {Request, Response, NextFunction} from "express";
import axios from "axios";
import config from "../../../config";

export const checkRoleAndProceed = (allowedRoles: string[]) => {
  return async (req: Request, res: Response, next: NextFunction) => {
    try {
      // Parse cookies from the request headers
      const cookies = req.headers.cookie;

      // Fetch the role from the backend (e.g., from the login route)
      const response = await axios.get(config.Backend1.URL + '/Login/role', {
        headers: {
          'Cookie': cookies // Manually set the Cookie header
        }
      });

      const role = response.data;

      console.log('Role:', role);

      if(allowedRoles.length === 0) {
        return next();
      }

      for (let i = 0; i < allowedRoles.length; i++) {
        allowedRoles[i] = allowedRoles[i].toLowerCase();
      }

      if (allowedRoles.includes(role.toLowerCase())) {
        return next();
      } else {
        return res.status(403).json({
          message: 'Forbidden: You do not have the necessary role to access this resource'
        });
      }
    } catch (error) {
      console.error('Error fetching role:', error.message);
      res.status(500).json({
        message: 'Error fetching role',
        error: error.message
      });
    }
  };
};
