import 'reflect-metadata';
import * as sinon from 'sinon';
import { Response, Request, NextFunction } from 'express';
import { Container } from 'typedi';
import IMedicalConditionService from '../../../../src/services/IServices/IMedicalConditionService';
import IMedicalConditionDTO from '../../../../src/dto/IMedicalConditionDTO';
import MedicalConditionController from "../../../../src/controllers/medicalConditionController";
import {AppError} from "../../../../src/domain/Shared/Exceptions/AppError";

describe('MedicalConditionController - Unit Test', function () {
    const sandbox = sinon.createSandbox();

    beforeEach(function () {
        Container.reset();

        const medicalConditionSchemaInstance = require('../../../../src/persistence/schemas/medicalConditionSchema').default;
        Container.set('medicalConditionSchema', medicalConditionSchemaInstance);

        const medicalConditionRepoClass = require('../../../../src/repos/MedicalConditionRepo').default;
        const medicalConditionRepoInstance = Container.get(medicalConditionRepoClass);
        Container.set('MedicalConditionRepo', medicalConditionRepoInstance);

        const medicalConditionServiceClass = require('../../../../src/services/medicalConditionService').default;
        const medicalConditionServiceInstance = Container.get(medicalConditionServiceClass);
        Container.set('MedicalConditionService', medicalConditionServiceInstance);
        
        const medicalConditionControllerClass = require('../../../../src/controllers/medicalConditionController').default;
        const medicalConditionControllerInstance = Container.get(medicalConditionControllerClass);
        Container.set('medicalConditionController', medicalConditionControllerInstance);
    });

    afterEach(function () {
        sandbox.restore();
    });

    it('should create a medical condition successfully using service stub', async function () {

        const body: IMedicalConditionDTO = {
            code: 'C123',
            designation: 'Hypertension',
            description: 'High blood pressure',
            symptomsList: ['Headache', 'Blurred vision'],
        };
        const req: Partial<Request> = { body };
        const res = {
            status: sinon.stub().returnsThis(),
            json: sinon.spy(),
        };
        const next: Partial<NextFunction> = () => {};

        const medicalConditionServiceInstance = Container.get('MedicalConditionService') as IMedicalConditionService;
        sinon.stub(medicalConditionServiceInstance, 'createMedicalCondition').resolves();

        const controller = new MedicalConditionController(medicalConditionServiceInstance);

        await controller.createMedicalCondition(req as Request, res as Response);
        
        expect(res.status.calledOnce).toBe(true);
        expect(res.status.calledWith(200)).toBe(true);
        expect(res.json.calledOnce).toBe(true);
        
    });

    it('should return an error if service throws AppError', async function () {

        const body: IMedicalConditionDTO = {
            code: 'C123',
            designation: 'Hypertension',
            description: 'High blood pressure',
            symptomsList: ['Headache', 'Blurred vision'],
        };
        const req: Partial<Request> = { body };
        const res= {
            status: sinon.stub().returnsThis(),
            json: sinon.spy(),
        };
        const next: Partial<NextFunction> = () => {};

        const medicalConditionServiceInstance = Container.get('MedicalConditionService') as IMedicalConditionService;
        const error = new AppError("CODE_NULL_UNDEFINED");
        sinon.stub(medicalConditionServiceInstance, 'createMedicalCondition').rejects(error);

        const controller = new MedicalConditionController(medicalConditionServiceInstance);

        await controller.createMedicalCondition(req as Request, res as Response);
        
        expect(res.status.calledOnce).toBe(true);
        expect(res.status.calledWith(error.code)).toBe(true);
        expect(res.json.calledOnce).toBe(true);
        expect(res.json.calledWith({ message: error.message })).toBe(true);
        
    });

    it('should return a 500 error for unexpected exceptions', async function () {

        const body: IMedicalConditionDTO = {
            code: 'C123',
            designation: 'Hypertension',
            description: 'High blood pressure',
            symptomsList: ['Headache', 'Blurred vision'],
        };
        const req: Partial<Request> = { body };
        const res = {
            status: sinon.stub().returnsThis(),
            json: sinon.spy(),
        };
        const next: Partial<NextFunction> = () => {};

        const medicalConditionServiceInstance = Container.get('MedicalConditionService') as IMedicalConditionService;
        const error = new Error('Unexpected error');
        sinon.stub(medicalConditionServiceInstance, 'createMedicalCondition').rejects(error);

        const controller = new MedicalConditionController(medicalConditionServiceInstance);

        await controller.createMedicalCondition(req as Request, res as Response);
        
        expect(res.status.calledOnce).toBe(true);
        expect(res.status.calledWith(500)).toBe(true);
        expect(res.json.calledOnce).toBe(true);
        expect(res.json.calledWith({ message: 'Code/Designation already exists' })).toBe(true);
        
    });
});
