import 'reflect-metadata';
import * as sinon from 'sinon';
import { Container } from 'typedi';
import MedicalConditionService from '../../../../src/services/medicalConditionService';
import IMedicalConditionRepo from '../../../../src/services/IRepos/IMedicalConditionRepo';
import { MedicalCondition } from '../../../../src/domain/MedicalCondition/MedicalCondition';
import IMedicalConditionDTO from '../../../../src/dto/IMedicalConditionDTO';

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
        { field: 'code', value: null },
        { field: 'designation', value: null },
        { field: 'description', value: null },
    ];

    nullFieldTestCases.forEach(({ field, value }) => {
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
});
