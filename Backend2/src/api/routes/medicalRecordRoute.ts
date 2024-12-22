import {Container} from "typedi";
import {Router} from "express";
import config from "../../../config";
import IMedicalRecordController from "../../controllers/IControllers/IMedicalRecordController";
import {celebrate, Joi} from "celebrate";
import {checkRoleAndProceed} from "../middlewares/validateUserRole";

const route = Router();

export default (app: Router) => {
    app.use('/medicalRecord', route);

    const ctrl = Container.get(config.controllers.medicalRecord.name) as IMedicalRecordController;

    route.post('/',
        celebrate({
            body: Joi.object({
                recordNumberId: Joi.string().required(),
            }),
        }),
        checkRoleAndProceed(['admin']),
        (req, res, next) => {
            ctrl.createMedicalRecord(req, res, next);
        });

    route.post('/freeText',
        celebrate({
            body: Joi.object({
                domainId: Joi.string().optional(),
                medicalRecordId: Joi.string().required(),
                doctorId: Joi.string().required(),
                comment: Joi.string().required()
            }),
        }),
       // checkRoleAndProceed(['doctor']),
        (req, res, next) => {
            ctrl.addFreeText(req, res, next);
        });

    route.get('/:id/allergy',
        //checkRoleAndProceed(['admin','doctor']),
        (req, res, next) => {
            ctrl.getAllergies(req, res, next);
        });
};
