import 'reflect-metadata';
import * as sinon from 'sinon';
import { Response, Request } from 'express';
import { Container } from 'typedi';
import MedicalConditionController from "../../../../src/controllers/medicalConditionController";
import IMedicalConditionService from "../../../../src/services/IServices/IMedicalConditionService";
import MedicalConditionService from "../../../../src/services/medicalConditionService";
import IMedicalConditionDTO from "../../../../src/dto/IMedicalConditionDTO";
import IMedicalConditionRepo from "../../../../src/services/IRepos/IMedicalConditionRepo";

describe('MedicalConditionController Integration Test', function () {
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

    it('should create a medical condition successfully', async function () {
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

        const medicalConditionRepo = Container.get('MedicalConditionRepo') as IMedicalConditionRepo;
        const saveStub = sinon.stub(medicalConditionRepo, 'save').resolves();
        const controller = Container.get<MedicalConditionController>('medicalConditionController');

        await controller.createMedicalCondition(req as Request, res as Response);
        
        sinon.assert.calledOnce(saveStub);
        sinon.assert.calledWith(
            saveStub,
            sinon.match((obj: any) =>
                obj.props.code.props.value === body.code &&
                obj.props.designation.props.value === body.designation &&
                obj.props.description.props.value === body.description &&
                JSON.stringify(obj.props.symptomsList) === JSON.stringify(body.symptomsList)
            ),
            sinon.match.any 
        );
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 200);
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, {
            message: 'Medical Condition created successfully',
        });
    });


    it('should return an error if a required property is invalid', async function () {
        const body: IMedicalConditionDTO = {
            code: '',
            designation: 'Hypertension',
            description: 'High blood pressure',
            symptomsList: ['Headache', 'Blurred vision'],
        };
        const req: Partial<Request> = { body };
        const res: Partial<Response> = {
            status: sinon.stub().returnsThis(),
            json: sinon.spy(),
        };

        const medicalConditionRepo = Container.get('MedicalConditionRepo') as IMedicalConditionRepo;
        sinon.stub(medicalConditionRepo, 'save').resolves();
        const controller = Container.get<MedicalConditionController>('medicalConditionController');

        await controller.createMedicalCondition(req as Request, res as Response);

        sinon.assert.notCalled(medicalConditionRepo.save); 
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 804);
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, {
            message: 'Code can not be empty.',
        });
    });

    it('should return an error if the designation is invalid', async function () {
        const body: IMedicalConditionDTO = {
            code: 'C123',
            designation: '',
            description: 'High blood pressure',
            symptomsList: ['Headache', 'Blurred vision'],
        };
        const req: Partial<Request> = { body };
        const res: Partial<Response> = {
            status: sinon.stub().returnsThis(),
            json: sinon.spy(),
        };

        const medicalConditionRepo = Container.get('MedicalConditionRepo') as IMedicalConditionRepo;
        sinon.stub(medicalConditionRepo, 'save').resolves();
        const controller = Container.get<MedicalConditionController>('medicalConditionController');

        await controller.createMedicalCondition(req as Request, res as Response);

        sinon.assert.notCalled(medicalConditionRepo.save);
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 806);
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, {
            message: 'Designation can not be empty.',
        });
    });

    it('should return an error if the description is invalid', async function () {
        const body: IMedicalConditionDTO = {
            code: 'C123',
            designation: 'Hypertension',
            description: null,
            symptomsList: ['Headache', 'Blurred vision'],
        };
        const req: Partial<Request> = { body };
        const res: Partial<Response> = {
            status: sinon.stub().returnsThis(),
            json: sinon.spy(),
        };

        const medicalConditionRepo = Container.get('MedicalConditionRepo') as IMedicalConditionRepo;
        sinon.stub(medicalConditionRepo, 'save').resolves();
        const controller = Container.get<MedicalConditionController>('medicalConditionController');

        await controller.createMedicalCondition(req as Request, res as Response);

        sinon.assert.notCalled(medicalConditionRepo.save);
        sinon.assert.calledOnce(res.status);
        sinon.assert.calledWith(res.status, 802);
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, {
            message: 'Description can not be null or undefined.',
        });
    });

});
