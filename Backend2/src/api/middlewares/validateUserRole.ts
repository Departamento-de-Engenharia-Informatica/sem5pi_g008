import {Request, Response, NextFunction} from "express";
import axios from "axios";
import config from "../../../config";

export const checkRoleAndProceed = (allowedRoles: string[]) => {
  return async (req: Request, res: Response, next: NextFunction) => {
    try {

      const response = await axios.get(config.Backend1.URL + '/Login/role', {
        headers: {
          'Cookie': req.headers.cookie
        }
      });

      const role = response.data;

      if(allowedRoles.length === 0) {
        return next();
      }

      const normalizedRoles = allowedRoles.map((r) => r.toLowerCase());

      if (normalizedRoles.includes(role.toLowerCase())) {
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
        error: error.message || 'Check if Backend1 is running'
      });
    }
  };
};
