import {Container} from "typedi";
import * as sinon from "sinon";
import IAllergyService from "../../../../src/services/IServices/IAllergyService";
import {Allergy} from "../../../../src/domain/Allergy/Allergy";
import {Designation} from "../../../../src/domain/Shared/designation";
import {Description} from "../../../../src/domain/Shared/description";
import {Code} from "../../../../src/domain/Shared/code";
import {UniqueEntityID} from "../../../../src/core/domain/UniqueEntityID";
import AllergyController from "../../../../src/controllers/AllergyController";
import IAllergyRepo from "../../../../src/services/IRepos/IAllergyRepo";
import exp from "node:constants";

describe('AllergyController - Integration', function () {

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
        Container.set('AllergyService', allergyServiceInstance);

        const allergyControllerClass = require('../../../../src/controllers/allergyController').default;
        const allergyControllerInstance = Container.get(allergyControllerClass);
        Container.set('allergyController', allergyControllerInstance);
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
            const req = {
                body: {
                    code: 'A123',
                    designation: 'Peanut Allergy',
                    description: 'Allergic to peanuts causing severe reactions',
                    effects: ['Anaphylaxis', 'Swelling', 'Hives']
                }
            };
            const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};

            const repoInstance = Container.get('AllergyRepo') as IAllergyRepo;
            const saveStub = sandbox.stub(repoInstance, 'save').resolves();
            const allergyController = Container.get('allergyController') as AllergyController;

            await allergyController.createAllergy(req, res);

            expect(saveStub.calledOnce).toBe(true);
            expect(saveStub.calledWith(
                sinon.match((obj: any) =>
                    obj.props.code.props.value === req.body.code &&
                    obj.props.designation.props.value === req.body.designation &&
                    obj.props.description.props.value === req.body.description &&
                    JSON.stringify(obj.props.effects) === JSON.stringify(req.body.effects)
                ),
            ))
            expect(res.status.calledWith(200)).toBe(true);
            expect(res.json.calledWith({message: 'Allergy created successfully'})).toBe(true);
        });

        it('should return an error if code is empty', async function () {
            const req = {
                body: {
                    code: '',
                    designation: 'Peanut Allergy',
                    description: 'Allergic to peanuts causing severe reactions',
                    effects: ['Anaphylaxis', 'Swelling', 'Hives']
                }
            };
            const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};

            const allergyController = Container.get('allergyController') as AllergyController;
            await allergyController.createAllergy(req, res);

            expect(res.status.calledWith(500)).toBe(true);
            expect(res.json.calledWith({message: 'Code can not be empty.'})).toBe(true);
        });

        it('should return an error if code is null', async function () {
            const req = {
                body: {
                    code: null,
                    designation: 'Peanut Allergy',
                    description: 'Allergic to peanuts causing severe reactions',
                    effects: ['Anaphylaxis', 'Swelling', 'Hives']
                }
            };
            const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};

            const allergyController = Container.get('allergyController') as AllergyController;
            await allergyController.createAllergy(req, res);

            expect(res.status.calledWith(500)).toBe(true);
            expect(res.json.calledWith({message: 'Code can not be null or undefined.'})).toBe(true);
        });

        it('should return an error if code is invalid', async function () {
            const req = {
                body: {
                    code: 'Z-123',
                    designation: 'Peanut Allergy',
                    description: 'Allergic to peanuts causing severe reactions',
                    effects: ['Anaphylaxis', 'Swelling', 'Hives']
                }
            };
            const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};

            const allergyController = Container.get('allergyController') as AllergyController;
            await allergyController.createAllergy(req, res);

            expect(res.status.calledWith(500)).toBe(true);
            expect(res.json.calledWith({message: 'Invalid ICD-11 code.'})).toBe(true);
        });

        it('should return an error if designation is empty', async function () {
            const req = {
                body: {
                    code: 'A123',
                    designation: '',
                    description: 'Allergic to peanuts causing severe reactions',
                    effects: ['Anaphylaxis', 'Swelling', 'Hives']
                }
            };
            const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};

            const allergyController = Container.get('allergyController') as AllergyController;
            await allergyController.createAllergy(req, res);

            expect(res.status.calledWith(500)).toBe(true);
            expect(res.json.calledWith({message: 'Designation can not be empty.'})).toBe(true);
        });

        it('should return an error if designation is null', async function () {
            const req = {
                body: {
                    code: 'A123',
                    designation: null,
                    description: 'Allergic to peanuts causing severe reactions',
                    effects: ['Anaphylaxis', 'Swelling', 'Hives']
                }
            };
            const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};

            const allergyController = Container.get('allergyController') as AllergyController;
            await allergyController.createAllergy(req, res);

            expect(res.status.calledWith(500)).toBe(true);
            expect(res.json.calledWith({message: 'Designation can not be null or undefined.'})).toBe(true);
        });

        it('should return an error if designation is invalid', async function () {
            const req = {
                body: {
                    code: 'A123',
                    designation: 'Allergy to natural rubber latex with cross-reactions to tropical fruits, such as banana, avocado, and kiwi.',
                    description: 'Allergic to peanuts causing severe reactions',
                    effects: ['Anaphylaxis', 'Swelling', 'Hives']
                }
            };
            const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};

            const allergyController = Container.get('allergyController') as AllergyController;
            await allergyController.createAllergy(req, res);

            expect(res.status.calledWith(500)).toBe(true);
            expect(res.json.calledWith({message: 'Designation must have a maximum of 100 characters.'})).toBe(true);
        });

        it('should return an error if description is null', async function () {
            const req = {
                body: {
                    code: 'A123',
                    designation: 'Peanut Allergy',
                    description: null,
                    effects: ['Anaphylaxis', 'Swelling', 'Hives']
                }
            };
            const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};

            const allergyController = Container.get('allergyController') as AllergyController;
            await allergyController.createAllergy(req, res);

            expect(res.status.calledWith(500)).toBe(true);
            expect(res.json.calledWith({message: 'Description can not be null or undefined.'})).toBe(true);
        });

        it('should return an error if description is invalid', async function () {
            const req = {
                body: {
                    code: 'A123',
                    designation: 'Peanut Allergy',
                    description: 'Allergy to natural rubber latex with associated hypersensitivity reactions and cross-reactivity to certain tropical fruits, including but not limited to banana, avocado, kiwi, chestnut, and passion fruit, which can manifest in diverse symptoms ranging from mild cutaneous reactions, such as localized itching, redness, and swelling, to more severe systemic responses, including respiratory distress, gastrointestinal discomfort, and anaphylaxis. This condition is often associated with a type I hypersensitivity reaction mediated by immunoglobulin E (IgE) antibodies that recognize specific proteins present in latex derived from the sap of the Hevea brasiliensis tree. The prevalence of latex-fruit syndrome, a subset of this allergy, is attributed to the structural similarity between latex proteins and proteins found in certain fruits, leading to an immune response when these allergens are encountered. Common triggers include exposure to latex products such as gloves, balloons, and medical equipment, which can cause direct contact reactions or airborne sensitization. Diagnosis typically involves a combination of clinical history, skin prick testing, and serological tests for specific IgE antibodies, while management focuses on strict avoidance of known allergens, the use of barrier protection methods, and the readiness to administer emergency treatments like epinephrine in cases of anaphylaxis. Education on the hidden sources of latex and cross-reactive fruits, alongside clear communication with healthcare providers and caretakers, is crucial for individuals living with this condition to mitigate risks and maintain quality of life. Research into alternative hypoallergenic materials and immunotherapy options continues to provide hope for improved management and potential desensitization protocols in the future.\'Allergy to natural rubber latex with associated hypersensitivity reactions and cross-reactivity to certain tropical fruits, including but not limited to banana, avocado, kiwi, chestnut, and passion fruit, which can manifest in diverse symptoms ranging from mild cutaneous reactions, such as localized itching, redness, and swelling, to more severe systemic responses, including respiratory distress, gastrointestinal discomfort, and anaphylaxis. This condition is often associated with a type I hypersensitivity reaction mediated by immunoglobulin E (IgE) antibodies that recognize specific proteins present in latex derived from the sap of the Hevea brasiliensis tree. The prevalence of latex-fruit syndrome, a subset of this allergy, is attributed to the structural similarity between latex proteins and proteins found in certain fruits, leading to an immune response when these allergens are encountered. Common triggers include exposure to latex products such as gloves, balloons, and medical equipment, which can cause direct contact reactions or airborne sensitization. Diagnosis typically involves a combination of clinical history, skin prick testing, and serological tests for specific IgE antibodies, while management focuses on strict avoidance of known allergens, the use of barrier protection methods, and the readiness to administer emergency treatments like epinephrine in cases of anaphylaxis. Education on the hidden sources of latex and cross-reactive fruits, alongside clear communication with healthcare providers and caretakers, is crucial for individuals living with this condition to mitigate risks and maintain quality of life. Research into alternative hypoallergenic materials and immunotherapy options continues to provide hope for improved management and potential desensitization protocols in the future.',
                    effects: ['Anaphylaxis', 'Swelling', 'Hives']
                }
            };
            const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};

            const allergyController = Container.get('allergyController') as AllergyController;
            await allergyController.createAllergy(req, res);

            expect(res.status.calledWith(500)).toBe(true);
            expect(res.json.calledWith({message: 'Description must have a maximum of 2048 characters.'})).toBe(true);
        });
    });

        describe('updateAllergyDesignation', function () {

            it('should update an allergy designation successfully', async function () {

                const req = {params: {id: '1'}, body: {designation: 'Nut Allergy'}};
                const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};


                const repoInstance = Container.get('AllergyRepo');
                const getByDomainIdStub = sandbox.stub(repoInstance, 'getByDomainId').resolves(createAllergy());
                const updateUsingDomainIdStub = sandbox.stub(repoInstance, 'updateUsingDomainId').resolves();

                const allergyController = Container.get('allergyController') as AllergyController;
                await allergyController.updateAllergyDesignation(req, res);

                expect(getByDomainIdStub.calledOnce).toBe(true);
                expect(res.status.calledWith(200)).toBe(true);
                expect(res.json.calledOnce).toBe(true);
                const updatedArgument = updateUsingDomainIdStub.firstCall.args[0];
                expect(updatedArgument).toBeInstanceOf(Allergy);

            });

            it('should fail to update an allergy designation if the designation is null', async function () {

                const req = {params: {id: '1'}, body: {designation: null}};
                const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};

                const repoInstance = Container.get('AllergyRepo');
                const getByDomainIdStub = sandbox.stub(repoInstance, 'getByDomainId').resolves(createAllergy());

                const allergyController = Container.get('allergyController') as AllergyController;
                await allergyController.updateAllergyDesignation(req, res);

                expect(getByDomainIdStub.calledOnce).toBe(true);
                expect(res.status.calledWith(803)).toBe(true);
                expect(res.json.calledOnce).toBe(true);
                expect(res.json.firstCall.args[0]).toEqual({message: 'Designation can not be null or undefined.'});
            });

            it('should fail to update an allergy designation if the designation is empty', async function () {

                const req = {params: {id: '1'}, body: {designation: " "}};
                const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};

                const repoInstance = Container.get('AllergyRepo');
                const getByDomainIdStub = sandbox.stub(repoInstance, 'getByDomainId').resolves(createAllergy());

                const allergyController = Container.get('allergyController') as AllergyController;
                await allergyController.updateAllergyDesignation(req, res);

                expect(getByDomainIdStub.calledOnce).toBe(true);
                expect(res.status.calledWith(806)).toBe(true);
                expect(res.json.calledOnce).toBe(true);
                expect(res.json.firstCall.args[0]).toEqual({message: 'Designation can not be empty.'});
            });
        });

        describe('updateAllergyDescription', function () {

            it('should update an allergy description successfully', async function () {

                const description = 'Allergic to nuts causing mild to severe reactions';

                const req = {params: {id: '1'}, body: {description: description}};
                const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};

                const repoInstance = Container.get('AllergyRepo');
                const getByDomainIdStub = sandbox.stub(repoInstance, 'getByDomainId').resolves(createAllergy());
                const updateUsingDomainIdStub = sandbox.stub(repoInstance, 'updateUsingDomainId').resolves();

                const allergyController = Container.get('allergyController') as AllergyController;
                await allergyController.updateAllergyDescription(req, res);

                expect(getByDomainIdStub.calledOnce).toBe(true);
                expect(res.status.calledWith(200)).toBe(true);
                expect(res.json.calledOnce).toBe(true);
                const updatedArgument = updateUsingDomainIdStub.firstCall.args[0];
                expect(updatedArgument).toBeInstanceOf(Allergy);
            });

            it('should fail to update an allergy description if the description is null', async function () {

                const description = null;

                const req = {params: {id: '1'}, body: {description: description}};
                const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};

                const repoInstance = Container.get('AllergyRepo');
                const getByDomainIdStub = sandbox.stub(repoInstance, 'getByDomainId').resolves(createAllergy());

                const allergyController = Container.get('allergyController') as AllergyController;
                await allergyController.updateAllergyDescription(req, res);

                expect(getByDomainIdStub.calledOnce).toBe(true);
                expect(res.status.calledWith(802)).toBe(true);
                expect(res.json.calledOnce).toBe(true);
                expect(res.json.firstCall.args[0]).toEqual({message: 'Description can not be null or undefined.'});
            });

        });

        describe('updateAllergyEffects', function () {

            it('should update an allergy effects successfully', async function () {

                const effects = ['Rashes', 'Difficulty Breathing'];
                const req = {params: {id: '1'}, body: {effects: effects}};
                const res = {status: sandbox.stub().returnsThis(), json: sandbox.stub()};

                const repoInstance = Container.get('AllergyRepo');
                const getByDomainIdStub = sandbox.stub(repoInstance, 'getByDomainId').resolves(createAllergy());
                const updateUsingDomainIdStub = sandbox.stub(repoInstance, 'updateUsingDomainId').resolves();

                const allergyController = Container.get('allergyController') as AllergyController;
                await allergyController.updateAllergyEffects(req, res);

                expect(getByDomainIdStub.calledOnce).toBe(true);
                expect(res.status.calledWith(200)).toBe(true);
                expect(res.json.calledOnce).toBe(true);
                const updatedArgument = updateUsingDomainIdStub.firstCall.args[0];
                expect(updatedArgument).toBeInstanceOf(Allergy);
            });
        });
    });
