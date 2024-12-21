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

  route.post('/',
    celebrate({
      body: Joi.object({
        domainId: Joi.number().optional(),
        allergy: Joi.string().required(),
        effect: Joi.string().optional()
      }),
    }),
    checkRoleAndProceed(['admin']), (req, res, next) => {
    ctrl.createAllergy(req, res, next);
  });

  route.get('/',
    checkRoleAndProceed(['admin','doctor']), (req, res, next) => {
    ctrl.getAllAllergies(req, res, next);
  });

};
