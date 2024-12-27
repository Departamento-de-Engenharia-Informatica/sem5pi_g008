import {Container} from "typedi";
import {Router} from "express";
import config from "../../../config";
import IMedicalRecordController from "../../controllers/IControllers/IMedicalRecordController";
import {celebrate, Joi} from "celebrate";
import {checkRoleAndProceed} from "../middlewares/validateUserRole";

const route = Router();

export default (app: Router) => {
    app.use('/familyhistory', route);

    const ctrl = Container.get(config.controllers.medicalRecord.name) as IMedicalRecordController;

    route.post('/'
        ,(req, res, next) => ctrl.createFamilyHistory(req, res, next),
    );

};

    