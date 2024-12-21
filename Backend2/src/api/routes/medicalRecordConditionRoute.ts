import {Container} from "typedi";
import {Router} from "express";
import config from "../../../config";
import IMedicalRecordConditionController from "../../controllers/IControllers/IMedicalRecordConditionController";

const route = Router();

export default (app: Router) => {
    app.use('/medicalRecordCondition', route);

    const ctrl = Container.get(config.controllers.medicalRecordCondition.name) as IMedicalRecordConditionController;

    route.get('/',
        (req, res, next) => {
            ctrl.getMedicalRecordConditions(req, res, next);
        }
    );
};
