import 'reflect-metadata';
import * as sinon from 'sinon';
import {Response, Request} from 'express';
import {Container} from 'typedi';
import MedicalConditionController from "../../../../src/controllers/MedicalConditionController";
import IMedicalConditionDTO from "../../../../src/dto/IMedicalConditionDTO";
import IMedicalConditionRepo from "../../../../src/services/IRepos/IMedicalConditionRepo";
import {Code} from "../../../../src/domain/Shared/code";
import {Designation} from "../../../../src/domain/Shared/designation";
import {Description} from "../../../../src/domain/Shared/description";
import {UniqueEntityID} from "../../../../src/core/domain/UniqueEntityID";
import {MedicalCondition} from "../../../../src/domain/MedicalCondition/MedicalCondition";


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

    function createMedicalCondition() {
        const props = {
            code: Code.create('C123').getValue(),
            designation: Designation.create('Hypertension').getValue(),
            description: Description.create('High blood pressure').getValue(),
            symptomsList: ['Headache', 'Blurred vision'],
        };

        const medicalConditionId = new UniqueEntityID('1');

        return MedicalCondition.create(props, medicalConditionId).getValue();
    }

    it('should create a medical condition successfully', async function () {
        const body: IMedicalConditionDTO = {
            code: 'C123',
            designation: 'Hypertension',
            description: 'High blood pressure',
            symptomsList: ['Headache', 'Blurred vision'],
        };
        const req: Partial<Request> = {body};
        const res = {
            status: sinon.stub().returnsThis(),
            json: sinon.spy(),
        };

        const medicalConditionRepo = Container.get('MedicalConditionRepo') as IMedicalConditionRepo;
        const saveStub = sinon.stub(medicalConditionRepo, 'save').resolves();
        const controller = Container.get<MedicalConditionController>('medicalConditionController');

        await controller.createMedicalCondition(req as Request, res as Response);

        expect(saveStub.calledOnce).toBe(true);
        expect(saveStub.calledWith(
            sinon.match((obj: any) =>
                obj.props.code.props.value === body.code &&
                obj.props.designation.props.value === body.designation &&
                obj.props.description.props.value === body.description &&
                JSON.stringify(obj.props.symptomsList) === JSON.stringify(body.symptomsList)
            ),
            sinon.match.any
        )).toBe(true);
        expect(res.status.calledWith(200)).toBe(true);
        expect(res.json.calledWith({message: 'Medical Condition created successfully'})).toBe(true);
    });


    it('should return an error if a required property is invalid', async function () {
        const body: IMedicalConditionDTO = {
            code: '',
            designation: 'Hypertension',
            description: 'High blood pressure',
            symptomsList: ['Headache', 'Blurred vision'],
        };
        const req: Partial<Request> = {body};
        const res = {
            status: sinon.stub().returnsThis(),
            json: sinon.spy(),
        };

        const medicalConditionRepo = Container.get('MedicalConditionRepo') as IMedicalConditionRepo;
        sinon.stub(medicalConditionRepo, 'save').resolves();
        const controller = Container.get<MedicalConditionController>('medicalConditionController');

        await controller.createMedicalCondition(req as Request, res as Response);

        expect(res.status.calledWith(804)).toBe(true);
        expect(res.json.calledWith({message: 'Code can not be empty.'})).toBe(true);
    });

    it('should return an error if the designation is invalid', async function () {
        const body: IMedicalConditionDTO = {
            code: 'C123',
            designation: '',
            description: 'High blood pressure',
            symptomsList: ['Headache', 'Blurred vision'],
        };
        const req: Partial<Request> = {body};
        const res = {
            status: sinon.stub().returnsThis(),
            json: sinon.spy(),
        };

        const medicalConditionRepo = Container.get('MedicalConditionRepo') as IMedicalConditionRepo;
        sinon.stub(medicalConditionRepo, 'save').resolves();
        const controller = Container.get<MedicalConditionController>('medicalConditionController');

        await controller.createMedicalCondition(req as Request, res as Response);

        expect(res.status.calledWith(806)).toBe(true);
        expect(res.json.calledWith({message: 'Designation can not be empty.'})).toBe(true);
    });

    it('should return an error if the description is invalid', async function () {
        const body: IMedicalConditionDTO = {
            code: 'C123',
            designation: 'Hypertension',
            description: null,
            symptomsList: ['Headache', 'Blurred vision'],
        };
        const req: Partial<Request> = {body};
        const res = {
            status: sinon.stub().returnsThis(),
            json: sinon.spy(),
        };

        const medicalConditionRepo = Container.get('MedicalConditionRepo') as IMedicalConditionRepo;
        sinon.stub(medicalConditionRepo, 'save').resolves();
        const controller = Container.get<MedicalConditionController>('medicalConditionController');

        await controller.createMedicalCondition(req as Request, res as Response);

        expect(res.status.calledWith(802)).toBe(true);
        expect(res.json.calledWith({message: 'Description can not be null or undefined.'})).toBe(true);
    });

    describe('updateMedicalConditionDescription', function () {
        it('should update the description of a medical condition successfully', async function () {
            const id = '1';
            const description = 'Updated description';

            const req: Partial<Request> = {params: {id: id}, body: {description: description}};
            const res = {
                status: sandbox.stub().returnsThis(),
                json: sandbox.stub(),
            };
            
            const repoInstance = Container.get('MedicalConditionRepo');
            const getByDomainIdStub = sandbox.stub(repoInstance, 'getByDomainId').resolves(createMedicalCondition());
            const updateUsingDomainIdStub = sandbox.stub(repoInstance, 'updateUsingDomainId').resolves();
            
            const controller = Container.get('medicalConditionController') as MedicalConditionController;

            await controller.updateMedicalConditionDescription(req, res);
           
            expect(getByDomainIdStub.calledOnce).toBe(true);
            expect(res.status.calledOnce).toBe(true);
            expect(res.json.calledOnce).toBe(true);
            expect(res.status.calledWith(200)).toBe(true);
            const updatedArgument = updateUsingDomainIdStub.firstCall.args[0];
            expect(updatedArgument.props.description.props.value).toBe(description);
            expect(res.json.calledWith({message: 'Medical Condition description updated successfully'})).toBe(true);
        });
            
        it('should fail to update the description if it is null', async function () {
            const id = '1';
            const description = null;

            const req: Partial<Request> = {params: {id: id}, body: {description: description}};
            const res = {
                status: sandbox.stub().returnsThis(),
                json: sandbox.stub(),
            };
            
            const repoInstance = Container.get('MedicalConditionRepo') as IMedicalConditionRepo;
            const getByDomainIdStub = sandbox.stub(repoInstance, 'getByDomainId').resolves(createMedicalCondition());
            

            const controller = Container.get<MedicalConditionController>('medicalConditionController');

            await controller.updateMedicalConditionDescription(req as Request, res as Response);

            expect(getByDomainIdStub.calledOnce).toBe(true);
            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(500)).toBe(true);
            expect(res.json.calledOnce).toBe(true);
            expect(res.json.calledWith({message: 'Description can not be null or undefined.'})).toBe(true);
        });
    });

    describe('updateMedicalConditionSymptoms', function () {
        it('should update the symptoms of a medical condition successfully', async function () {
            const id = '1';
            const symptomsList = ['Updated symptom 1', 'Updated symptom 2'];

            const req: Partial<Request> = {params: {id: id}, body: {symptomsList: symptomsList}};
            const res = {
                status: sandbox.stub().returnsThis(),
                json: sandbox.stub(),
            };

            const medicalConditionRepo = Container.get('MedicalConditionRepo') as IMedicalConditionRepo;
            const getByDomainIdStub = sandbox.stub(medicalConditionRepo, 'getByDomainId').resolves(createMedicalCondition());
            const updateUsingDomainIdStub = sandbox.stub(medicalConditionRepo, 'updateUsingDomainId').resolves();
           
            const controller = Container.get('medicalConditionController') as MedicalConditionController;
            await controller.updateMedicalConditionSymptoms(req as Request, res as Response);

            expect(getByDomainIdStub.calledOnce).toBe(true);
            expect(getByDomainIdStub.calledWith(Number.parseInt(id))).toBe(true);
            expect(updateUsingDomainIdStub.calledOnce).toBe(true);
            expect(updateUsingDomainIdStub.calledWith(
                sandbox.match((obj: any) => JSON.stringify(obj.props.symptomsList) === JSON.stringify(symptomsList)),
                'symptomsList'
            )).toBe(true);
            expect(res.status.calledWith(200)).toBe(true);
            expect(res.json.calledWith({message: 'Medical Condition symptoms updated successfully'})).toBe(true);
        });
    });
});

