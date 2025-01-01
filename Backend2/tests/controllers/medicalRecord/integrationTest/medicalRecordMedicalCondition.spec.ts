import 'reflect-metadata';
import * as sinon from 'sinon';
import {Container} from 'typedi';
import IMedicalRecordService from "../../../../src/services/IServices/IMedicalRecordService";
import {MedicalRecord} from "../../../../src/domain/MedicalRecord/MedicalRecord";
import {MedicalRecordCondition} from "../../../../src/domain/MedicalRecordCondition/MedicalRecordCondition";
import {MedicalCondition} from "../../../../src/domain/MedicalCondition/MedicalCondition";
import {Code} from "../../../../src/domain/Shared/code";
import {Designation} from "../../../../src/domain/Shared/designation";
import {Description} from "../../../../src/domain/Shared/description";
import {UniqueEntityID} from "../../../../src/core/domain/UniqueEntityID";
import MedicalRecordController from "../../../../src/controllers/medicalRecordController";

describe('MedicalRecordController - MedicalRecordCondition - Integration', function () {
    const sandbox = sinon.createSandbox();

    beforeEach(function () {
        Container.reset();

        const medicalRecordSchemaInstance = require('../../../../src/persistence/schemas/medicalRecordSchema').default;
        Container.set('medicalRecordSchema', medicalRecordSchemaInstance);

        const medicalRecordAllergySchemaInstance = require('../../../../src/persistence/schemas/medicalRecordAllergySchema').default;
        Container.set('medicalRecordAllergySchema', medicalRecordAllergySchemaInstance);

        const allergySchemaInstance = require('../../../../src/persistence/schemas/allergySchema').default;
        Container.set('allergySchema', allergySchemaInstance);

        const medicalRecordFreeTextSchemaInstance = require('../../../../src/persistence/schemas/medicalRecordFreeTextSchema').default;
        Container.set('medicalRecordFreeTextSchema', medicalRecordFreeTextSchemaInstance);

        const medicalRecordMedicalConditionSchemaInstance = require('../../../../src/persistence/schemas/medicalRecordConditionSchema').default;
        Container.set('medicalRecordConditionSchema', medicalRecordMedicalConditionSchemaInstance);

        const medicalConditionSchemaInstance = require('../../../../src/persistence/schemas/medicalConditionSchema').default;
        Container.set('medicalConditionSchema', medicalConditionSchemaInstance);

        const medicalRecordRepoClass = require('../../../../src/repos/MedicalRecordRepo').default;
        const medicalRecordRepoInstance = Container.get(medicalRecordRepoClass);
        Container.set('MedicalRecordRepo', medicalRecordRepoInstance);

        const medicalRecordAllergyRepoClass = require('../../../../src/repos/MedicalRecordAllergyRepo').default;
        const medicalRecordAllergyRepoInstance = Container.get(medicalRecordAllergyRepoClass);
        Container.set('MedicalRecordAllergyRepo', medicalRecordAllergyRepoInstance);

        const allergyRepoClass = require('../../../../src/repos/AllergyRepo').default;
        const allergyRepoInstance = Container.get(allergyRepoClass);
        Container.set('AllergyRepo', allergyRepoInstance);

        const medicalRecordFreeTextRepoClass = require('../../../../src/repos/MedicalRecordFreeTextRepo').default;
        const medicalRecordFreeTextRepoInstance = Container.get(medicalRecordFreeTextRepoClass);
        Container.set('MedicalRecordFreeTextRepo', medicalRecordFreeTextRepoInstance);

        const medicalRecordConditionRepoClass = require('../../../../src/repos/MedicalRecordConditionRepo').default;
        const medicalRecordConditionRepoInstance = Container.get(medicalRecordConditionRepoClass);
        Container.set('MedicalRecordConditionRepo', medicalRecordConditionRepoInstance);

        const medicalConditionRepoClass = require('../../../../src/repos/MedicalConditionRepo').default;
        const medicalConditionRepoInstance = Container.get(medicalConditionRepoClass);
        Container.set('MedicalConditionRepo', medicalConditionRepoInstance);

        const medicalRecordServiceClass = require('../../../../src/services/medicalRecordService').default;
        const medicalRecordServiceInstance = Container.get(medicalRecordServiceClass);
        Container.set('MedicalRecordService', medicalRecordServiceInstance);

        const medicalRecordControllerClass = require('../../../../src/controllers/medicalRecordController').default;
        const medicalRecordControllerInstance = Container.get(medicalRecordControllerClass);
        Container.set('MedicalRecordController', medicalRecordControllerInstance);

    });

    afterEach(function () {
        sandbox.restore();
    });

    function createMedicalRecordCondition() {
        const medicalRecordConditionProps = {
            condition: 'test-condition',
            medicalRecord: 'test-id',
            doctorId: 'test-doctor',
            comment: 'test-comment'
        };
        return MedicalRecordCondition.create(medicalRecordConditionProps).getValue();
    }

    function createMedicalRecord() {
        const medicalRecordProps = {
            _id: new UniqueEntityID().toString(),
        };
        return MedicalRecord.create(medicalRecordProps).getValue();
    }

    function createMedicalCondition() {
        const medicalConditionProps = {
            _id: new UniqueEntityID().toString(),
            code: Code.create('C10').getValue(),
            designation: Designation.create('test-designation').getValue(),
            description: Description.create('test-description').getValue(),
            symptomsList: ['test-symptom']
        };
        return MedicalCondition.create(medicalConditionProps).getValue();
    }

    describe('getMedicalRecordConditions', function () {

        it('should return NoMedicalRecordException', async function () {

            const req = {params: {id: "test-id"}};
            const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};

            const medicalRecordRepoMock = sandbox.stub(Container.get('MedicalRecordRepo'));
            medicalRecordRepoMock.getMedicalRecordByDomainId.resolves(null);

            const medicalRecordController = Container.get("MedicalRecordController") as MedicalRecordController;

            await medicalRecordController.getMedicalRecordConditions(req, res);

            expect(res.status.calledWith(900)).toBe(true);
            expect(res.json.calledWith({message: "No medical record found."})).toBe(true);
        });

        it('should return MedicalRecordConditionNotFoundException', async function () {

            const req = {params: {id: "test-id"}};
            const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};

            const medicalRecordRepoMock = sandbox.stub(Container.get('MedicalRecordRepo'));
            medicalRecordRepoMock.getMedicalRecordByDomainId.resolves(createMedicalRecord());

            const medicalRecordConditionRepoMock = sandbox.stub(Container.get('MedicalRecordConditionRepo'));
            medicalRecordConditionRepoMock.getMedicalRecordConditionsWithIds.resolves([]);

            const medicalRecordController = Container.get("MedicalRecordController") as MedicalRecordController;

            await medicalRecordController.getMedicalRecordConditions(req, res);

            expect(res.status.calledWith(850)).toBe(true);
            expect(res.json.calledWith({message: "No conditions found for this medical record."})).toBe(true);
        });

        it('should return medical record conditions', async function () {

            const req = {params: {id: "test-id"}};
            const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};

            const medicalRecordRepoMock = sandbox.stub(Container.get('MedicalRecordRepo'));
            medicalRecordRepoMock.getMedicalRecordByDomainId.resolves(createMedicalRecord());

            const medicalRecordConditionRepoMock = sandbox.stub(Container.get('MedicalRecordConditionRepo'));
            medicalRecordConditionRepoMock.getMedicalRecordConditionsWithIds.resolves([createMedicalRecordCondition()]);

            const medicalRecordService = Container.get("MedicalRecordService") as IMedicalRecordService;
            const getStaffDetailsStub = sandbox.stub(medicalRecordService, 'getStaffDetails');
            getStaffDetailsStub.resolves({
                firstName: 'John',
                lastName: 'Doe',
                licenseNumber: 'D101'
            });

            const medicalConditionRepoMock = sandbox.stub(Container.get('MedicalConditionRepo'));
            medicalConditionRepoMock.getMedicalConditionByBusinessId.resolves(createMedicalCondition());

            const medicalRecordController = Container.get("MedicalRecordController") as MedicalRecordController;

            await medicalRecordController.getMedicalRecordConditions(req, res);

            const result = res.json.args[0][0].medicalRecordConditions;

            expect(result.length).toBe(1);
            expect(result[0].doctorName).toBe('John Doe');
            expect(result[0].conditionDesignation).toBe('test-designation');
            expect(result[0].comment).toBe('test-comment');
            expect(res.status.calledWith(200)).toBe(true);
        });
    });

    describe('getMedicalRecordConditionByCode', function () {

        it('should return medical record conditions by code', async function () {

            const req = {params: {id: "test-id", code: "C10"}};
            const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};

            const medicalRecordRepoMock = sandbox.stub(Container.get('MedicalRecordRepo'));
            medicalRecordRepoMock.getMedicalRecordByDomainId.resolves(createMedicalRecord());

            const medicalConditionRepoMock = sandbox.stub(Container.get('MedicalConditionRepo'));
            medicalConditionRepoMock.getMedicalConditionByCode.resolves(createMedicalCondition())

            const medicalRecordConditionRepoMock = sandbox.stub(Container.get('MedicalRecordConditionRepo'));
            medicalRecordConditionRepoMock.getMedicalRecordConditionByMedicalRecordIdAndConditionId.resolves(createMedicalRecordCondition());

            const medicalRecordService = Container.get("MedicalRecordService") as IMedicalRecordService;
            const getStaffDetailsStub = sandbox.stub(medicalRecordService, 'getStaffDetails');
            getStaffDetailsStub.resolves({
                firstName: 'John',
                lastName: 'Doe',
                licenseNumber: 'D101'
            });

            const medicalRecordController = Container.get("MedicalRecordController") as MedicalRecordController;

            await medicalRecordController.getMedicalRecordConditionByCode(req, res);

            const result = res.json.args[0][0].medicalRecordCondition;

            expect(result.doctorName).toBe('John Doe');
            expect(result.conditionDesignation).toBe('test-designation');
            expect(result.comment).toBe('test-comment');
            expect(result.conditionCode).toBe('C10');
            expect(res.status.calledWith(200)).toBe(true);
        });

        it('should return MedicalRecordConditionNotFoundException searching by code', async function () {

            const medicalRecordRepoMock = sandbox.stub(Container.get('MedicalRecordRepo'));
            medicalRecordRepoMock.getMedicalRecordByDomainId.resolves(createMedicalRecord());

            const medicalConditionRepoMock = sandbox.stub(Container.get('MedicalConditionRepo'));
            medicalConditionRepoMock.getMedicalConditionByCode.resolves(createMedicalCondition())

            const medicalRecordConditionRepoMock = sandbox.stub(Container.get('MedicalRecordConditionRepo'));
            medicalRecordConditionRepoMock.getMedicalRecordConditionByMedicalRecordIdAndConditionId.resolves(null);

            const medicalRecordController = Container.get("MedicalRecordController") as MedicalRecordController;

            const req = {params: {id: "test-id", code: "C10"}};
            const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};

            await medicalRecordController.getMedicalRecordConditionByCode(req, res);

            expect(res.status.calledWith(851)).toBe(true);
            expect(res.json.calledWith({message: "No Medical Condition found for this Code."})).toBe(true);
        });

        it('should return MedicalConditionNotFoundException searching by code', async function () {

            const req = {params: {id: "test-id", code: "C10"}};
            const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};

            const medicalRecordRepoMock = sandbox.stub(Container.get('MedicalRecordRepo'));
            medicalRecordRepoMock.getMedicalRecordByDomainId.resolves(createMedicalRecord());

            const medicalConditionRepoMock = sandbox.stub(Container.get('MedicalConditionRepo'));
            medicalConditionRepoMock.getMedicalConditionByCode.resolves(null)

            const medicalRecordController = Container.get("MedicalRecordController") as MedicalRecordController;
            await medicalRecordController.getMedicalRecordConditionByCode(req, res);

            expect(res.status.calledWith(810)).toBe(true);
            expect(res.json.calledWith({message: "No Medical Condition registered in the system with this Code."})).toBe(true);

        });

    });

    describe('getMedicalRecordConditionByCode', function () {

        it('should return medical record conditions by designation', async function () {
            
            const req = {params: {id: "test-id", designation: "test-designation"}};
            const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};

            const medicalRecordRepoMock = sandbox.stub(Container.get('MedicalRecordRepo'));
            medicalRecordRepoMock.getMedicalRecordByDomainId.resolves(createMedicalRecord());

            const medicalConditionRepoMock = sandbox.stub(Container.get('MedicalConditionRepo'));
            medicalConditionRepoMock.getMedicalConditionByDesignation.resolves(createMedicalCondition())

            const medicalRecordConditionRepoMock = sandbox.stub(Container.get('MedicalRecordConditionRepo'));
            medicalRecordConditionRepoMock.getMedicalRecordConditionByMedicalRecordIdAndConditionId.resolves(createMedicalRecordCondition());

            const medicalRecordService = Container.get("MedicalRecordService") as IMedicalRecordService;
            const getStaffDetailsStub = sandbox.stub(medicalRecordService, 'getStaffDetails');
            getStaffDetailsStub.resolves({
                firstName: 'John',
                lastName: 'Doe',
                licenseNumber: 'D101'
            });
            
            const medicalRecordController = Container.get("MedicalRecordController") as MedicalRecordController;
            await medicalRecordController.getMedicalRecordConditionByDesignation(req, res);
            
            const result = res.json.args[0][0].medicalRecordCondition;

            expect(result.doctorName).toBe('John Doe');
            expect(result.conditionDesignation).toBe('test-designation');
            expect(result.comment).toBe('test-comment');
            expect(res.status.calledWith(200)).toBe(true);
        });

        it('should return MedicalRecordConditionNotFoundException searching by designation', async function () {

            const req = {params: {id: "test-id", designation: "test-designation"}};
            const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};
            
            const medicalRecordRepoMock = sandbox.stub(Container.get('MedicalRecordRepo'));
            medicalRecordRepoMock.getMedicalRecordByDomainId.resolves(createMedicalRecord());

            const medicalConditionRepoMock = sandbox.stub(Container.get('MedicalConditionRepo'));
            medicalConditionRepoMock.getMedicalConditionByDesignation.resolves(createMedicalCondition())

            const medicalRecordConditionRepoMock = sandbox.stub(Container.get('MedicalRecordConditionRepo'));
            medicalRecordConditionRepoMock.getMedicalRecordConditionByMedicalRecordIdAndConditionId.resolves(null);

            const medicalRecordController = Container.get("MedicalRecordController") as MedicalRecordController;
            await medicalRecordController.getMedicalRecordConditionByDesignation(req, res);
            
            expect(res.status.calledWith(851)).toBe(true);
            expect(res.json.calledWith({message: "No Medical Condition found for this Designation."})).toBe(true);
        });

        it('should return MedicalConditionNotFoundException searching by designation', async function () {
            
            const req = {params: {id: "test-id", designation: "test-designation"}};
            const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};
            
            const medicalRecordService = Container.get("MedicalRecordService") as IMedicalRecordService;

            const medicalRecordRepoMock = sandbox.stub(Container.get('MedicalRecordRepo'));
            medicalRecordRepoMock.getMedicalRecordByDomainId.resolves(createMedicalRecord());

            const medicalConditionRepoMock = sandbox.stub(Container.get('MedicalConditionRepo'));
            medicalConditionRepoMock.getMedicalConditionByDesignation.resolves(null)

            const medicalRecordController = Container.get("MedicalRecordController") as MedicalRecordController;
            await medicalRecordController.getMedicalRecordConditionByDesignation(req, res);
            
            expect(res.status.calledWith(810)).toBe(true);
            expect(res.json.calledWith({message: "No Medical Condition registered in the system with this Designation."})).toBe(true);
        });
        
    });

});