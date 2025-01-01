import {Container} from "typedi";
import * as sinon from "sinon";
import IAllergyService from "../../../../src/services/IServices/IAllergyService";
import {Allergy} from "../../../../src/domain/Allergy/Allergy";
import {Designation} from "../../../../src/domain/Shared/designation";
import {Description} from "../../../../src/domain/Shared/description";
import {Code} from "../../../../src/domain/Shared/code";
import {UniqueEntityID} from "../../../../src/core/domain/UniqueEntityID";
import AllergyController from "../../../../src/controllers/allergyController";

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

            console.log(res.json.firstCall.args[0]);
            console.log(res);
            
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