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
                code: Joi.string().required(),
                designation: Joi.string().required(),
                description: Joi.string().required(),
                effects: Joi.array().items(Joi.string()).required(),
                isDeleted: Joi.boolean().optional(),
            }),
        }),
        //checkRoleAndProceed(['admin']),
           (req, res, next) => {
            ctrl.createAllergy(req, res, next);
        });

    route.get('/',
        checkRoleAndProceed(['admin', 'doctor']), (req, res, next) => {
            ctrl.getAllAllergies(req, res, next);
        });

    route.get('/:filter',
        (req, res, next) => {
            ctrl.searchAllergies(req, res, next);
        });
    
    route.patch('/:id/designation',
        celebrate({
            params: Joi.object({
                id: Joi.string().required(),
            }),
            body: Joi.object({
                designation: Joi.string().required(),
            }),
        }),
        //checkRoleAndProceed(['admin']),
        (req, res, next) => {
            ctrl.updateAllergyDesignation(req, res, next);
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
        //checkRoleAndProceed(['admin']),
        (req, res, next) => {
            ctrl.updateAllergyDescription(req, res, next);
        });

    route.patch('/:id/effects',
        celebrate({
            params: Joi.object({
                id: Joi.string().required(),
            }),
            body: Joi.object({
                effects: Joi.array().items(Joi.string()).required(),
            }),
        }),
        //checkRoleAndProceed(['admin']),
        (req, res, next) => {
            ctrl.updateAllergyEffects(req, res, next);
        });
    
    //TODO POR CHECK ROLE AND PROCEED

};
