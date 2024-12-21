import {Container} from "typedi";
import {Router} from "express";
import config from "../../../config";
import {celebrate, Joi} from "celebrate";
import {checkRoleAndProceed} from "../middlewares/validateUserRole";
import IMedicalRecordConditionController from "../../controllers/IControllers/IMedicalRecordConditionController";

const route = Router();

export default (app: Router) => {
    app.use('/medicalRecordCondition', route);

    const ctrl = Container.get(config.controllers.medicalRecordMedicalCondition.name) as IMedicalRecordConditionController;

    route.get(
        '/',
        celebrate({
            query: Joi.object({
                recordNumberId: Joi.string().required(), // Validação no query
            }),
        }),
        (req, res, next) => {
            ctrl.getMedicalRecordConditions(req, res, next);
        }
    );
};
