import 'reflect-metadata';
import * as sinon from 'sinon';
import { Container } from 'typedi';
import IMedicalRecordService from "../../../../src/services/IServices/IMedicalRecordService";
import {MedicalRecord} from "../../../../src/domain/MedicalRecord/MedicalRecord";
import {MedicalRecordCondition} from "../../../../src/domain/MedicalRecordCondition/MedicalRecordCondition";
import {MedicalCondition} from "../../../../src/domain/MedicalCondition/MedicalCondition";
import {Code} from "../../../../src/domain/Shared/code";
import {Designation} from "../../../../src/domain/Shared/designation";
import {Description} from "../../../../src/domain/Shared/description";
import {UniqueEntityID} from "../../../../src/core/domain/UniqueEntityID";

describe('MedicalRecordService - MedicalRecordCondition', function () {
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
        return  MedicalRecordCondition.create(medicalRecordConditionProps).getValue();
    }
    
    function createMedicalRecord(){
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
        return  MedicalCondition.create(medicalConditionProps).getValue();
    }
    
    it('should return NoMedicalRecordException', async function () {
        const medicalRecordService = Container.get("MedicalRecordService") as IMedicalRecordService;

        const medicalRecordRepoMock = sandbox.stub(Container.get('MedicalRecordRepo'));
        medicalRecordRepoMock.getMedicalRecordByDomainId.resolves(null);

        try {
            await medicalRecordService.getMedicalRecordConditions('test-id');
        } catch (e) {
            expect(e.message).toBe('No medical record found.');
        }
    });
    
    it('should return MedicalRecordConditionNotFoundException', async function () {
        const medicalRecordService = Container.get("MedicalRecordService") as IMedicalRecordService;

        const medicalRecordRepoMock = sandbox.stub(Container.get('MedicalRecordRepo'));
        medicalRecordRepoMock.getMedicalRecordByDomainId.resolves(createMedicalRecord());

        const medicalRecordConditionRepoMock = sandbox.stub(Container.get('MedicalRecordConditionRepo'));
        medicalRecordConditionRepoMock.getMedicalRecordConditionsWithIds.resolves([]);

        try {
            await medicalRecordService.getMedicalRecordConditions('test-id');
        } catch (e) {
            expect(e.message).toBe('No conditions found for this medical record.');
        }
    });

    it('should return medical record conditions', async function () {
        const medicalRecordService = Container.get("MedicalRecordService") as IMedicalRecordService;

        const medicalRecordRepoMock = sandbox.stub(Container.get('MedicalRecordRepo'));
        medicalRecordRepoMock.getMedicalRecordByDomainId.resolves(createMedicalRecord());

        const medicalRecordConditionRepoMock = sandbox.stub(Container.get('MedicalRecordConditionRepo'));
        medicalRecordConditionRepoMock.getMedicalRecordConditionsWithIds.resolves([createMedicalRecordCondition()]);

        const getStaffDetailsStub = sandbox.stub(medicalRecordService, 'getStaffDetails');
        getStaffDetailsStub.resolves({
            firstName: 'John',
            lastName: 'Doe',
            licenseNumber: 'D101'
        });
        
        const medicalConditionRepoMock = sandbox.stub(Container.get('MedicalConditionRepo'));
        medicalConditionRepoMock.getMedicalConditionByBusinessId.resolves(createMedicalCondition());

        const result = await medicalRecordService.getMedicalRecordConditions('test-id');

        expect(result.length).toBe(1);
        expect(result[0].doctorName).toBe('John Doe');
        expect(result[0].conditionDesignation).toBe('test-designation');
        expect(result[0].comment).toBe('test-comment');
    });

    it('should return medical record conditions by code', async function () {
        const medicalRecordService = Container.get("MedicalRecordService") as IMedicalRecordService;

        const medicalRecordRepoMock = sandbox.stub(Container.get('MedicalRecordRepo'));
        medicalRecordRepoMock.getMedicalRecordByDomainId.resolves(createMedicalRecord());

        const medicalConditionRepoMock = sandbox.stub(Container.get('MedicalConditionRepo'));
        medicalConditionRepoMock.getMedicalConditionByCode.resolves(createMedicalCondition())
        
        const medicalRecordConditionRepoMock = sandbox.stub(Container.get('MedicalRecordConditionRepo'));
        medicalRecordConditionRepoMock.getMedicalRecordConditionByMedicalRecordIdAndConditionId.resolves(createMedicalRecordCondition());

        const getStaffDetailsStub = sandbox.stub(medicalRecordService, 'getStaffDetails');
        getStaffDetailsStub.resolves({
            firstName: 'John',
            lastName: 'Doe',
            licenseNumber: 'D101'
        });

        const result = await medicalRecordService.getMedicalRecordConditionByCode('test-id',"C10");
        
        expect(result.doctorName).toBe('John Doe');
        expect(result.conditionDesignation).toBe('test-designation');
        expect(result.comment).toBe('test-comment');
        expect(result.conditionCode).toBe('C10');
    });
    
    it('should return MedicalRecordConditionNotFoundException searching by code', async function () {
        const medicalRecordService = Container.get("MedicalRecordService") as IMedicalRecordService;

        const medicalRecordRepoMock = sandbox.stub(Container.get('MedicalRecordRepo'));
        medicalRecordRepoMock.getMedicalRecordByDomainId.resolves(createMedicalRecord());

        const medicalConditionRepoMock = sandbox.stub(Container.get('MedicalConditionRepo'));
        medicalConditionRepoMock.getMedicalConditionByCode.resolves(createMedicalCondition())
        
        const medicalRecordConditionRepoMock = sandbox.stub(Container.get('MedicalRecordConditionRepo'));
        medicalRecordConditionRepoMock.getMedicalRecordConditionByMedicalRecordIdAndConditionId.resolves(null);

        try {
            await medicalRecordService.getMedicalRecordConditionByCode('test-id',"C10");
        } catch (e) {
            expect(e.message).toBe('No Medical Condition found for this Code.');
        }
    });
    
    it('should return MedicalConditionNotFoundException searching by code', async function () {
        const medicalRecordService = Container.get("MedicalRecordService") as IMedicalRecordService;

        const medicalRecordRepoMock = sandbox.stub(Container.get('MedicalRecordRepo'));
        medicalRecordRepoMock.getMedicalRecordByDomainId.resolves(createMedicalRecord());

        const medicalConditionRepoMock = sandbox.stub(Container.get('MedicalConditionRepo'));
        medicalConditionRepoMock.getMedicalConditionByCode.resolves(null)
        
        try {
            await medicalRecordService.getMedicalRecordConditionByCode('test-id',"C10");
        } catch (e) {
            expect(e.message).toBe('No Medical Condition registered in the system with this Code.');
        }
    });
    
    it('should return medical record conditions by designation', async function () {
        const medicalRecordService = Container.get("MedicalRecordService") as IMedicalRecordService;

        const medicalRecordRepoMock = sandbox.stub(Container.get('MedicalRecordRepo'));
        medicalRecordRepoMock.getMedicalRecordByDomainId.resolves(createMedicalRecord());

        const medicalConditionRepoMock = sandbox.stub(Container.get('MedicalConditionRepo'));
        medicalConditionRepoMock.getMedicalConditionByDesignation.resolves(createMedicalCondition())
        
        const medicalRecordConditionRepoMock = sandbox.stub(Container.get('MedicalRecordConditionRepo'));
        medicalRecordConditionRepoMock.getMedicalRecordConditionByMedicalRecordIdAndConditionId.resolves(createMedicalRecordCondition());

        const getStaffDetailsStub = sandbox.stub(medicalRecordService, 'getStaffDetails');
        getStaffDetailsStub.resolves({
            firstName: 'John',
            lastName: 'Doe',
            licenseNumber: 'D101'
        });

        const result = await medicalRecordService.getMedicalRecordConditionByDesignation('test-id',"test-designation");

        expect(result.doctorName).toBe('John Doe');
        expect(result.conditionDesignation).toBe('test-designation');
        expect(result.comment).toBe('test-comment');
    });
    
    it('should return MedicalRecordConditionNotFoundException searching by designation', async function () {
        const medicalRecordService = Container.get("MedicalRecordService") as IMedicalRecordService;

        const medicalRecordRepoMock = sandbox.stub(Container.get('MedicalRecordRepo'));
        medicalRecordRepoMock.getMedicalRecordByDomainId.resolves(createMedicalRecord());

        const medicalConditionRepoMock = sandbox.stub(Container.get('MedicalConditionRepo'));
        medicalConditionRepoMock.getMedicalConditionByDesignation.resolves(createMedicalCondition())
        
        const medicalRecordConditionRepoMock = sandbox.stub(Container.get('MedicalRecordConditionRepo'));
        medicalRecordConditionRepoMock.getMedicalRecordConditionByMedicalRecordIdAndConditionId.resolves(null);

        try {
            await medicalRecordService.getMedicalRecordConditionByDesignation('test-id',"test-designation");
        } catch (e) {
            expect(e.message).toBe('No Medical Condition found for this Designation.');
        }
    });
    
    it('should return MedicalConditionNotFoundException searching by designation', async function () {
        const medicalRecordService = Container.get("MedicalRecordService") as IMedicalRecordService;

        const medicalRecordRepoMock = sandbox.stub(Container.get('MedicalRecordRepo'));
        medicalRecordRepoMock.getMedicalRecordByDomainId.resolves(createMedicalRecord());

        const medicalConditionRepoMock = sandbox.stub(Container.get('MedicalConditionRepo'));
        medicalConditionRepoMock.getMedicalConditionByDesignation.resolves(null)
        
        try {
            await medicalRecordService.getMedicalRecordConditionByDesignation('test-id',"test-designation");
        } catch (e) {
            expect(e.message).toBe('No Medical Condition registered in the system with this Designation.');
        }
    });
    
});
