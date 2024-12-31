import 'reflect-metadata';
import * as sinon from 'sinon';
import { Response, Request, NextFunction } from 'express';
import { Container } from 'typedi';
import IMedicalConditionService from '../../../../src/services/IServices/IMedicalConditionService';
import IMedicalConditionDTO from '../../../../src/dto/IMedicalConditionDTO';
import MedicalConditionController from "../../../../src/controllers/medicalConditionController";
import {AppError} from "../../../../src/domain/Shared/Exceptions/AppError";

describe('MedicalConditionController', function () {
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
        // Arrange
        const body: IMedicalConditionDTO = {
            code: 'C123',
            designation: 'Hypertension',
            description: 'High blood pressure',
            symptomsList: ['Headache', 'Blurred vision'],
        };
        const req: Partial<Request> = { body };
        const res: Partial<Response> = {
            status: sinon.stub().returnsThis(),
            json: sinon.spy(),
        };
        const next: Partial<NextFunction> = () => {};

        const medicalConditionServiceInstance = Container.get('MedicalConditionService') as IMedicalConditionService;
        sinon.stub(medicalConditionServiceInstance, 'createMedicalCondition').resolves();

        const controller = new MedicalConditionController(medicalConditionServiceInstance);

        await controller.createMedicalCondition(req as Request, res as Response);

        sinon.assert.calledOnce(medicalConditionServiceInstance.createMedicalCondition);
        sinon.assert.calledWith(medicalConditionServiceInstance.createMedicalCondition, body);
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, {
            message: 'Medical Condition created successfully',
        });
    });

    it('should return an error if service throws AppError', async function () {

        const body: IMedicalConditionDTO = {
            code: 'C123',
            designation: 'Hypertension',
            description: 'High blood pressure',
            symptomsList: ['Headache', 'Blurred vision'],
        };
        const req: Partial<Request> = { body };
        const res: Partial<Response> = {
            status: sinon.stub().returnsThis(),
            json: sinon.spy(),
        };
        const next: Partial<NextFunction> = () => {};

        const medicalConditionServiceInstance = Container.get('MedicalConditionService') as IMedicalConditionService;
        const error = new AppError("CODE_NULL_UNDEFINED");
        sinon.stub(medicalConditionServiceInstance, 'createMedicalCondition').rejects(error);

        const controller = new MedicalConditionController(medicalConditionServiceInstance);

        await controller.createMedicalCondition(req as Request, res as Response);

        // 
        sinon.assert.calledOnce(medicalConditionServiceInstance.createMedicalCondition);
        sinon.assert.calledWith(medicalConditionServiceInstance.createMedicalCondition, body);
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, error.code);
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, { message: error.message });
        
    });

    it('should return a 500 error for unexpected exceptions', async function () {
        // Arrange
        const body: IMedicalConditionDTO = {
            code: 'C123',
            designation: 'Hypertension',
            description: 'High blood pressure',
            symptomsList: ['Headache', 'Blurred vision'],
        };
        const req: Partial<Request> = { body };
        const res: Partial<Response> = {
            status: sinon.stub().returnsThis(),
            json: sinon.spy(),
        };
        const next: Partial<NextFunction> = () => {};

        const medicalConditionServiceInstance = Container.get('MedicalConditionService') as IMedicalConditionService;
        const error = new Error('Unexpected error');
        sinon.stub(medicalConditionServiceInstance, 'createMedicalCondition').rejects(error);

        const controller = new MedicalConditionController(medicalConditionServiceInstance);

        // Act
        await controller.createMedicalCondition(req as Request, res as Response);

        // Assert
        sinon.assert.calledOnce(medicalConditionServiceInstance.createMedicalCondition);
        sinon.assert.calledWith(medicalConditionServiceInstance.createMedicalCondition, body);
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 500);
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, {
            message: 'Code/Designation already exists',
        });
    });
});
