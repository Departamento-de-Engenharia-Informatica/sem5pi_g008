import {Container} from "typedi";
import * as sinon from "sinon";
import IAllergyService from "../../../../src/services/IServices/IAllergyService";
import {Allergy} from "../../../../src/domain/Allergy/Allergy";
import {Designation} from "../../../../src/domain/Shared/designation";
import {Description} from "../../../../src/domain/Shared/description";
import {Code} from "../../../../src/domain/Shared/code";
import {AllergyId} from "../../../../src/domain/Allergy/AllergyId";
import {UniqueEntityID} from "../../../../src/core/domain/UniqueEntityID";

describe('AllergyService', function () {

    const sandbox = sinon.createSandbox();

    beforeEach(function () {
        Container.reset();
        const allergySchemaInstance = require('../../../../src/persistence/schemas/allergySchema').default;
        Container.set('allergySchema', allergySchemaInstance);

        const allergyRepoClass = require('../../../../src/repos/AllergyRepo').default;
        const allergyRepoInstance = Container.get(allergyRepoClass);
        Container.set('AllergyRepo', allergyRepoInstance);

        const allergyServiceClass = require('../../../../src/services/allergyService').default;
        const allergyServiceInstance = Container.get(allergyServiceClass);
        Container.set('allergyService', allergyServiceInstance);
    });

    afterEach(function () {
        sandbox.restore();
    });

    function createAllergy() {
        const allergyProps = {
            code: Code.create('A123').getValue(),
            designation: Designation.create('Peanut Allergy').getValue(),
            description: Description.create('Allergic to peanuts causing severe reactions').getValue(),
            effects: ['Anaphylaxis', 'Swelling', 'Hives'],
        };
        const allergyId = new UniqueEntityID(1);

        return Allergy.create(allergyProps, allergyId).getValue();
    }

    describe('createAllergy', function () {

        it('should create an allergy successfully', async function () {
            const allergyDTO = {
                code: 'A123',
                designation: 'Peanut Allergy',
                description: 'Allergic to peanuts causing severe reactions',
                effects: ['Anaphylaxis', 'Swelling', 'Hives'],
            };

            // Arrange
            const repoInstance = Container.get('AllergyRepo');
            const saveStub = sandbox.stub(repoInstance, 'save').resolves();
            const allergyService = Container.get('allergyService') as IAllergyService;

            // Act
            await allergyService.createAllergy(allergyDTO);

            // Assert
            expect(saveStub.calledOnce).toBe(true);
            const savedArgument = saveStub.firstCall.args[0];
            expect(savedArgument).toBeInstanceOf(Allergy);

            saveStub.restore();
        });

        it('should create an allergy successfully', async function () {
            const allergyDTO = {
                code: 'A-123',
                designation: 'Peanut Allergy',
                description: 'Allergic to peanuts causing severe reactions',
                effects: ['Anaphylaxis', 'Swelling', 'Hives'],
            };

            // Arrange
            const repoInstance = Container.get('AllergyRepo');
            const saveStub = sandbox.stub(repoInstance, 'save').resolves();
            const allergyService = Container.get('allergyService') as IAllergyService;

            // Act
            try {
                await allergyService.createAllergy(allergyDTO);
            } catch (e) {
                expect(e.message).toBe('Invalid ICD-11 code.');
            }

            saveStub.restore();
        });

        const nullFieldTestCases = [
            {field: 'code', value: null},
            {field: 'designation', value: null},
            {field: 'description', value: null},
        ];

        nullFieldTestCases.forEach(({field, value}) => {
            it(`should fail to save if ${field} is null`, async function () {
                const allergyDTO = {
                    code: 'A123',
                    designation: 'Peanut Allergy',
                    description: 'Allergic to peanuts causing severe reactions',
                    effects: ['Anaphylaxis', 'Swelling', 'Hives'],
                };

                (allergyDTO as any)[field] = value;

                const repoInstance = Container.get('AllergyRepo');
                const saveStub = sandbox.stub(repoInstance, 'save').resolves();
                const allergyService = Container.get('allergyService') as IAllergyService;

                try {
                    await allergyService.createAllergy(allergyDTO);
                    fail(`Expected an error to be thrown for invalid ${field}`);
                } catch (error: any) {
                    expect(error.message.toLowerCase()).toContain(`${field} can not be null or undefined`.toLowerCase());
                }

                expect(saveStub.called).toBe(false);
                saveStub.restore();
            });
        });

    });

    describe('updateAllergyDesignation', function () {

        it('should update an allergy designation successfully', async function () {

            const id = '1';
            const designation = 'Nut Allergy';


            const repoInstance = Container.get('AllergyRepo');
            const getByDomainIdStub = sandbox.stub(repoInstance, 'getByDomainId').resolves(createAllergy());
            const updateUsingDomainIdStub = sandbox.stub(repoInstance, 'updateUsingDomainId').resolves();
            const allergyService = Container.get('allergyService') as IAllergyService;

            // Act
            await allergyService.updateAllergyDesignation(id, designation);

            // Assert
            expect(getByDomainIdStub.calledOnce).toBe(true);
            expect(updateUsingDomainIdStub.calledOnce).toBe(true);
            const updatedArgument = updateUsingDomainIdStub.firstCall.args[0];
            expect(updatedArgument).toBeInstanceOf(Allergy);

        });

        it('should fail to update an allergy designation if the designation is invalid', async function () {

            const id = '1';
            const designation = ' ';

            const repoInstance = Container.get('AllergyRepo');
            const getByDomainIdStub = sandbox.stub(repoInstance, 'getByDomainId').resolves(createAllergy());
            const updateUsingDomainIdStub = sandbox.stub(repoInstance, 'updateUsingDomainId').resolves();
            const allergyService = Container.get('allergyService') as IAllergyService;

            try {
                await allergyService.updateAllergyDesignation(id, designation);
                fail('Expected an error to be thrown for invalid designation');
            } catch (error: any) {
                expect(error.message).toBe('Designation can not be empty.');
            }

            expect(getByDomainIdStub.calledOnce).toBe(true);
            expect(updateUsingDomainIdStub.called).toBe(false);
        });
    });
    
    describe('updateAllergyDescription', function () {
        
        it('should update an allergy description successfully', async function () {
            const id = '1';
            const description = 'Allergic to nuts causing mild to severe reactions';

            const repoInstance = Container.get('AllergyRepo');
            const getByDomainIdStub = sandbox.stub(repoInstance, 'getByDomainId').resolves(createAllergy());
            const updateUsingDomainIdStub = sandbox.stub(repoInstance, 'updateUsingDomainId').resolves();
            const allergyService = Container.get('allergyService') as IAllergyService;

            await allergyService.updateAllergyDescription(id, description);

            expect(getByDomainIdStub.calledOnce).toBe(true);
            expect(updateUsingDomainIdStub.calledOnce).toBe(true);
            const updatedArgument = updateUsingDomainIdStub.firstCall.args[0];
            expect(updatedArgument).toBeInstanceOf(Allergy);
        });

        it('should fail to update an allergy description if the description is invalid', async function () {
            const id = '1';
            const description = null;

            const repoInstance = Container.get('AllergyRepo');
            const getByDomainIdStub = sandbox.stub(repoInstance, 'getByDomainId').resolves(createAllergy());
            const updateUsingDomainIdStub = sandbox.stub(repoInstance, 'updateUsingDomainId').resolves();
            const allergyService = Container.get('allergyService') as IAllergyService;

            try {
                await allergyService.updateAllergyDescription(id, description);
                fail('Expected an error to be thrown for invalid description');
            } catch (error: any) {
                expect(error.message).toBe('Description can not be null or undefined.');
            }

            expect(getByDomainIdStub.calledOnce).toBe(true);
            expect(updateUsingDomainIdStub.called).toBe(false);
        });
        
    });
    
    describe('updateAllergyEffects', function () {

        it('should update an allergy effects successfully', async function () {
            const id = '1';
            const effects = ['Rashes', 'Difficulty Breathing'];

            const repoInstance = Container.get('AllergyRepo');
            const getByDomainIdStub = sandbox.stub(repoInstance, 'getByDomainId').resolves(createAllergy());
            const updateUsingDomainIdStub = sandbox.stub(repoInstance, 'updateUsingDomainId').resolves();
            const allergyService = Container.get('allergyService') as IAllergyService;

            // Act
            await allergyService.updateAllergyEffects(id, effects);

            // Assert
            expect(getByDomainIdStub.calledOnce).toBe(true);
            expect(updateUsingDomainIdStub.calledOnce).toBe(true);
            const updatedArgument = updateUsingDomainIdStub.firstCall.args[0];
            expect(updatedArgument).toBeInstanceOf(Allergy);
        });
    });
});