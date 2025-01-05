import 'reflect-metadata';
import * as sinon from 'sinon';
import {Container} from 'typedi';
import MedicalConditionService from '../../../../src/services/medicalConditionService';
import IMedicalConditionRepo from '../../../../src/services/IRepos/IMedicalConditionRepo';
import {MedicalCondition} from '../../../../src/domain/MedicalCondition/MedicalCondition';
import IMedicalConditionDTO from '../../../../src/dto/IMedicalConditionDTO';
import {Code} from "../../../../src/domain/Shared/code";
import {Designation} from "../../../../src/domain/Shared/designation";
import {Description} from "../../../../src/domain/Shared/description";
import {UniqueEntityID} from "../../../../src/core/domain/UniqueEntityID";
import exp from "node:constants";

describe('MedicalConditionService', function () {
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
        Container.set('medicalConditionService', medicalConditionServiceInstance);
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

    it('should create a medical condition successfully using service mock', async function () {
        const medicalConditionDTO: IMedicalConditionDTO = {
            code: 'C123',
            designation: 'Hypertension',
            description: 'High blood pressure',
            symptomsList: ['Headache', 'Blurred vision'],
        };

        // Arrange
        const repoInstance = Container.get('MedicalConditionRepo') as IMedicalConditionRepo;
        const saveStub = sinon.stub(repoInstance, 'save').resolves();
        const medicalConditionService = Container.get('medicalConditionService') as MedicalConditionService;

        // Act
        await medicalConditionService.createMedicalCondition(medicalConditionDTO);

        // Assert
        expect(saveStub.calledOnce).toBe(true);
        const savedArgument = saveStub.firstCall.args[0];
        expect(savedArgument).toBeInstanceOf(MedicalCondition);

        saveStub.restore();
    });

    const nullFieldTestCases = [
        {field: 'code', value: null},
        {field: 'designation', value: null},
        {field: 'description', value: null},
    ];

    nullFieldTestCases.forEach(({field, value}) => {
        it(`should fail to save if ${field} is null`, async function () {
            const medicalConditionDTO: IMedicalConditionDTO = {
                code: 'C123',
                designation: 'Hypertension',
                description: 'High blood pressure',
                symptomsList: ['Headache', 'Blurred vision'],
            };

            (medicalConditionDTO as any)[field] = value;

            const repoInstance = Container.get('MedicalConditionRepo') as IMedicalConditionRepo;
            const saveStub = sinon.stub(repoInstance, 'save').resolves();
            const medicalConditionService = Container.get('medicalConditionService') as MedicalConditionService;

            try {
                await medicalConditionService.createMedicalCondition(medicalConditionDTO);
                fail(`Expected an error to be thrown for invalid ${field}`);
            } catch (error: any) {
                expect(error.message.toLowerCase()).toContain(`${field} can not be null or undefined`.toLowerCase());
            }

            expect(saveStub.called).toBe(false);
            saveStub.restore();
        });
    });

    describe('updateMedicalConditionDescription', function () {
        it('should update the description of a medical condition successfully', async function () {

            const repoInstance = Container.get('MedicalConditionRepo');
            const getByDomainIdStub = sinon.stub(repoInstance, 'getByDomainId').resolves(createMedicalCondition());
            const updateUsingDomainIdStub = sinon.stub(repoInstance, 'updateUsingDomainId').resolves();
            const medicalConditionService = Container.get('medicalConditionService') as MedicalConditionService;

            await medicalConditionService.updateMedicalConditionDescription('1', 'New description');

            expect(getByDomainIdStub.calledOnce).toBe(true);
            expect(updateUsingDomainIdStub.calledOnce).toBe(true);
            const updatedArgument = updateUsingDomainIdStub.firstCall.args[0];
            expect(updatedArgument).toBeInstanceOf(MedicalCondition);
            expect(updatedArgument.description.value).toBe('New description');
        });

        it('should fail to update the description of a medical condition if the description is null', async function () {
            const repoInstance = Container.get('MedicalConditionRepo');
            const getByDomainIdStub = sinon.stub(repoInstance, 'getByDomainId').resolves(createMedicalCondition());
            const updateUsingDomainIdStub = sinon.stub(repoInstance, 'updateUsingDomainId').resolves();
            const medicalConditionService = Container.get('medicalConditionService') as MedicalConditionService;

            try {
                await medicalConditionService.updateMedicalConditionDescription('1', null);
                fail('Expected an error to be thrown for invalid description');
            } catch (error: any) {
                expect(error.message.toLowerCase()).toContain('description can not be null or undefined'.toLowerCase());
            }

            expect(getByDomainIdStub.called).toBe(true);
            expect(updateUsingDomainIdStub.called).toBe(false);
        });
    });

    describe('updateMedicalConditionSymptoms', function () {
        it('should update the symptoms of a medical condition successfully', async function () {
            const repoInstance = Container.get('MedicalConditionRepo');
            const getByDomainIdStub = sinon.stub(repoInstance, 'getByDomainId').resolves(createMedicalCondition());
            const updateUsingDomainIdStub = sinon.stub(repoInstance, 'updateUsingDomainId').resolves();
            const medicalConditionService = Container.get('medicalConditionService') as MedicalConditionService;

            await medicalConditionService.updateMedicalConditionSymptoms('1', ['New symptom']);

            expect(getByDomainIdStub.calledOnce).toBe(true);
            expect(updateUsingDomainIdStub.calledOnce).toBe(true);
            const updatedArgument = updateUsingDomainIdStub.firstCall.args[0];
            expect(updatedArgument.symptomsList).toEqual(['New symptom']);
        });
    });

});
