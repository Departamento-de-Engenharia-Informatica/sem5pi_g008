import {Router} from "express";
import {celebrate, Joi} from "celebrate";

import {Container} from "typedi";
import IMedicalConditionController from "../../controllers/IControllers/IMedicalConditionController";

import config from "../../../config";
import {checkRoleAndProceed} from "../middlewares/validateUserRole";

const route = Router();

export default (app: Router) => {
  app.use('/medicalConditions', route);

  const ctrl = Container.get(config.controllers.medicalCondition.name) as IMedicalConditionController;

  route.post('/',
    celebrate({
      body: Joi.object({
        code: Joi.string().required(),
        designation: Joi.string().required(),
        description: Joi.string().optional(),
        symptomsList: Joi.array().items(Joi.string()).optional(),
      }),
    }),
    checkRoleAndProceed(['admin']), (req, res, next) => {
      ctrl.createMedicalCondition(req, res, next);
    });

  route.get('/',
    checkRoleAndProceed(['admin']),
    (req, res, next) => {
      ctrl.getAllMedicalConditions(req, res, next);
    });

  route.patch('/:id/description',
    celebrate({
      params: Joi.object({
        id: Joi.string().required(),
      }),
      body: Joi.object({
        description: Joi.string().required(),
      }),
    }),
    checkRoleAndProceed(['admin']),
    (req, res, next) => {
      ctrl.updateMedicalConditionDescription(req, res, next);
    });

  route.patch('/:id/symptoms',
    celebrate({
      params: Joi.object({
        id: Joi.string().required(),
      }),
      body: Joi.object({
        symptomsList: Joi.array().items(Joi.string()).required(),
      }),
    }),
    checkRoleAndProceed(['admin']),
    (req, res, next) => {
      ctrl.updateMedicalConditionSymptoms(req, res, next);
    });

    route.get('/code/:code',
        (req, res, next) => {
            console.log("I am here", req.params.code);
            next();  
        },
        (req, res, next) => ctrl.searchMedicalConditionsCode(req, res, next)
    );
    route.get('/designation/:designation',
        (req, res, next) => {
            console.log("I am here");
            next();
        }, 
        (req, res, next) => ctrl.searchMedicalConditionsDesignation(req, res, next)
    );
 
};   
               