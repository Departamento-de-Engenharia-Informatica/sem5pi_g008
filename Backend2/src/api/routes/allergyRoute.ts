import {Router} from "express";
import {celebrate, Joi} from "celebrate";
import {checkRoleAndProceed} from "../middlewares/validateUserRole";

import {Container} from "typedi";
import IAllergyController from "../../controllers/IControllers/IAllergyController";

import config from "../../../config";

const route = Router();

export default (app: Router) => {
  app.use('/allergy', route);

  const ctrl = Container.get(config.controllers.allergy.name) as IAllergyController;

  route.get('/test',checkRoleAndProceed(['doctor']), (req, res) => {
    res.status(200).json({
      message: 'Access granted to route',
    });
  });
};
