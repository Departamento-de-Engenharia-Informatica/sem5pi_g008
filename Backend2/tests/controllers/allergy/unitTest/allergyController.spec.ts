import 'reflect-metadata';
import * as sinon from 'sinon';
import { Request, Response, NextFunction } from 'express';
import { Container } from 'typedi';
import IAllergyService from '../../../../src/services/IServices/IAllergyService';
import { NotFoundException } from '../../../../src/Exceptions/NotFoundException';
import { AppError } from '../../../../src/domain/Shared/Exceptions/AppError';
import AllergyController from "../../../../src/controllers/AllergyController";
import IAllergyDTO from "../../../../src/dto/IAllergyDTO";

describe('AllergyController - Unit Test', function () {
    const sandbox = sinon.createSandbox();

    beforeEach(function () {
        Container.reset();
        
        const allergySchemaInstance = require('../../../../src/persistence/schemas/allergySchema').default;
        Container.set('allergySchema', allergySchemaInstance);

        const allergyRepoClass = require('../../../../src/repos/AllergyRepo').default;
        const allergyRepoInstance = Container.get(allergyRepoClass);
        Container.set('AllergyRepo', allergyRepoInstance);

        const allergyServiceClass = require('../../../../src/services/AllergyService').default;
        const allergyServiceInstance = Container.get(allergyServiceClass);
        Container.set('AllergyService', allergyServiceInstance);

        const allergyControllerClass = require('../../../../src/controllers/AllergyController').default;
        const allergyControllerInstance = Container.get(allergyControllerClass);
        Container.set('AllergyController', allergyControllerInstance);
    });

    afterEach(function () {
        sandbox.restore();
    });
    
    
    describe('createAllergy', function () {

        it('should create an allergy successfully using service stub', async function () {

            const body: IAllergyDTO = {
                code: 'A123',
                designation: 'Peanut Allergy',
                description: 'Allergic to peanuts causing severe reactions',
                effects: ['Anaphylaxis', 'Swelling', 'Hives'],
            };

            const req: Partial<Request> = {body};
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };
            const next: Partial<NextFunction> = () => {
            };

            const allergyServiceInstance = Container.get('AllergyService') as IAllergyService;
            sinon.stub(allergyServiceInstance, 'createAllergy').resolves();

            const controller = new AllergyController(allergyServiceInstance);

            await controller.createAllergy(req as Request, res as Response);

            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(200)).toBe(true);
            expect(res.json.calledOnce).toBe(true);
            expect(res.json.calledWith({message: 'Allergy created successfully'})).toBe(true);
        });

        it('should return a 500 error for unexpected exceptions', async function () {
            const body: IAllergyDTO = {
                code: 'A123',
                designation: 'Peanut Allergy',
                description: 'Allergic to peanuts causing severe reactions',
                effects: ['Anaphylaxis', 'Swelling', 'Hives'],
            };

            const req: Partial<Request> = {body};
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const allergyServiceInstance = Container.get('AllergyService') as IAllergyService;
            const error = new Error('Unexpected error');
            sinon.stub(allergyServiceInstance, 'createAllergy').rejects(error);

            const controller = new AllergyController(allergyServiceInstance);

            await controller.createAllergy(req as Request, res as Response);

            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(500)).toBe(true);
            expect(res.json.calledOnce).toBe(true);
            expect(res.json.calledWith({message: 'Error creating allergy - Duplicate entry'})).toBe(true);
        });

        it('should return an error for Empty code', async function () {
            const body: IAllergyDTO = {
                code: '',
                designation: 'Peanut Allergy',
                description: 'Allergic to peanuts causing severe reactions',
                effects: ['Anaphylaxis', 'Swelling', 'Hives'],
            };

            const req: Partial<Request> = {body};
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const allergyServiceInstance = Container.get('AllergyService') as IAllergyService;
            const appError = new AppError("CODE_INVALID_WHITESPACE");
            sinon.stub(allergyServiceInstance, 'createAllergy').rejects(appError);

            const controller = new AllergyController(allergyServiceInstance);

            await controller.createAllergy(req as Request, res as Response);

            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(500)).toBe(true);
            const jsonArg = res.json.getCall(0).args[0];
            expect(jsonArg.message).toBe(appError.message);
        });
        
        it('should return an error for Invalid code', async function () {
            const body: IAllergyDTO = {
                code: 'A1234',
                designation: 'Peanut Allergy',
                description: 'Allergic to peanuts causing severe reactions',
                effects: ['Anaphylaxis', 'Swelling', 'Hives'],
            };

            const req: Partial<Request> = {body};
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const allergyServiceInstance = Container.get('AllergyService') as IAllergyService;
            const appError = new AppError("INVALID_ICD11_CODE");
            sinon.stub(allergyServiceInstance, 'createAllergy').rejects(appError);

            const controller = new AllergyController(allergyServiceInstance);

            await controller.createAllergy(req as Request, res as Response);

            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(500)).toBe(true);
            const jsonArg = res.json.getCall(0).args[0]; 
            expect(jsonArg.message).toBe(appError.message);
        });

        it('should return an error for Null code', async function () {
            const body: IAllergyDTO = {
                code: null,
                designation: 'Peanut Allergy',
                description: 'Allergic to peanuts causing severe reactions',
                effects: ['Anaphylaxis', 'Swelling', 'Hives'],
            };

            const req: Partial<Request> = {body};
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const allergyServiceInstance = Container.get('AllergyService') as IAllergyService;
            const appError = new AppError("CODE_NULL_UNDEFINED");
            sinon.stub(allergyServiceInstance, 'createAllergy').rejects(appError);

            const controller = new AllergyController(allergyServiceInstance);

            await controller.createAllergy(req as Request, res as Response);

            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(500)).toBe(true);
            const jsonArg = res.json.getCall(0).args[0];
            expect(jsonArg.message).toBe(appError.message);
        });

        it('should return an error for Empty Designation', async function () {
            const body: IAllergyDTO = {
                code: 'A12',
                designation: '',
                description: 'Allergic to peanuts causing severe reactions',
                effects: ['Anaphylaxis', 'Swelling', 'Hives'],
            };

            const req: Partial<Request> = {body};
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const allergyServiceInstance = Container.get('AllergyService') as IAllergyService;
            const appError = new AppError("DESIGNATION_INVALID_WHITESPACE");
            sinon.stub(allergyServiceInstance, 'createAllergy').rejects(appError);

            const controller = new AllergyController(allergyServiceInstance);

            await controller.createAllergy(req as Request, res as Response);

            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(500)).toBe(true);
            const jsonArg = res.json.getCall(0).args[0];
            expect(jsonArg.message).toBe(appError.message);
        });
        
        it('should return an error for Null Designation', async function () {
            const body: IAllergyDTO = {
                code: 'A12',
                designation: null,
                description: 'Allergic to peanuts causing severe reactions',
                effects: ['Anaphylaxis', 'Swelling', 'Hives'],
            };

            const req: Partial<Request> = {body};
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const allergyServiceInstance = Container.get('AllergyService') as IAllergyService;
            const appError = new AppError("DESIGNATION_NULL_UNDEFINED");
            sinon.stub(allergyServiceInstance, 'createAllergy').rejects(appError);

            const controller = new AllergyController(allergyServiceInstance);

            await controller.createAllergy(req as Request, res as Response);

            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(500)).toBe(true);
            const jsonArg = res.json.getCall(0).args[0];
            expect(jsonArg.message).toBe(appError.message);
        });

        it('should return an error for Invalid Designation', async function () {
            const body: IAllergyDTO = {
                code: 'A12',
                designation: 'Allergy to natural rubber latex with cross-reactions to tropical fruits, such as banana, avocado, and kiwi.',
                description: 'Allergic to peanuts causing severe reactions',
                effects: ['Anaphylaxis', 'Swelling', 'Hives'],
            };

            const req: Partial<Request> = {body};
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const allergyServiceInstance = Container.get('AllergyService') as IAllergyService;
            const appError = new AppError("DESIGNATION_INVALID_LENGTH");
            sinon.stub(allergyServiceInstance, 'createAllergy').rejects(appError);

            const controller = new AllergyController(allergyServiceInstance);

            await controller.createAllergy(req as Request, res as Response);

            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(500)).toBe(true);
            const jsonArg = res.json.getCall(0).args[0];
            expect(jsonArg.message).toBe(appError.message);
        });

        it('should return an error for Null Description', async function () {
            const body: IAllergyDTO = {
                code: 'A12',
                designation: 'Peanut Allergy',
                description: null,
                effects: ['Anaphylaxis', 'Swelling', 'Hives'],
            };

            const req: Partial<Request> = {body};
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const allergyServiceInstance = Container.get('AllergyService') as IAllergyService;
            const appError = new AppError("DESCRIPTION_NULL_UNDEFINED");
            sinon.stub(allergyServiceInstance, 'createAllergy').rejects(appError);

            const controller = new AllergyController(allergyServiceInstance);

            await controller.createAllergy(req as Request, res as Response);

            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(500)).toBe(true);
            const jsonArg = res.json.getCall(0).args[0];
            expect(jsonArg.message).toBe(appError.message);
        });

        it('should return an error for Invalid Description', async function () {
            const body: IAllergyDTO = {
                code: 'A12',
                designation: 'Peanut Allergy',
                description: 'Allergy to natural rubber latex with associated hypersensitivity reactions and cross-reactivity to certain tropical fruits, including but not limited to banana, avocado, kiwi, chestnut, and passion fruit, which can manifest in diverse symptoms ranging from mild cutaneous reactions, such as localized itching, redness, and swelling, to more severe systemic responses, including respiratory distress, gastrointestinal discomfort, and anaphylaxis. This condition is often associated with a type I hypersensitivity reaction mediated by immunoglobulin E (IgE) antibodies that recognize specific proteins present in latex derived from the sap of the Hevea brasiliensis tree. The prevalence of latex-fruit syndrome, a subset of this allergy, is attributed to the structural similarity between latex proteins and proteins found in certain fruits, leading to an immune response when these allergens are encountered. Common triggers include exposure to latex products such as gloves, balloons, and medical equipment, which can cause direct contact reactions or airborne sensitization. Diagnosis typically involves a combination of clinical history, skin prick testing, and serological tests for specific IgE antibodies, while management focuses on strict avoidance of known allergens, the use of barrier protection methods, and the readiness to administer emergency treatments like epinephrine in cases of anaphylaxis. Education on the hidden sources of latex and cross-reactive fruits, alongside clear communication with healthcare providers and caretakers, is crucial for individuals living with this condition to mitigate risks and maintain quality of life. Research into alternative hypoallergenic materials and immunotherapy options continues to provide hope for improved management and potential desensitization protocols in the future.',
                effects: ['Anaphylaxis', 'Swelling', 'Hives'],
            };

            const req: Partial<Request> = {body};
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const allergyServiceInstance = Container.get('AllergyService') as IAllergyService;
            const appError = new AppError("DESCRIPTION_INVALID_LENGTH");
            sinon.stub(allergyServiceInstance, 'createAllergy').rejects(appError);

            const controller = new AllergyController(allergyServiceInstance);

            await controller.createAllergy(req as Request, res as Response);

            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(500)).toBe(true);
            const jsonArg = res.json.getCall(0).args[0];
            expect(jsonArg.message).toBe(appError.message);
        });
    });

    /**
     * Tests for updateAllergyDesignation
     */
    describe('updateAllergyDesignation', () => {
        it('should successfully update an allergy designation', async function () {
            const req: Partial<Request> = {
                params: { id: '1' },
                body: { designation: 'Updated Allergy Designation' },
            };
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const allergyServiceInstance = Container.get('AllergyService') as IAllergyService;
            const updatedAllergy = { id: '1', designation: 'Updated Allergy Designation' };
            sinon.stub(allergyServiceInstance, 'updateAllergyDesignation').resolves(updatedAllergy);

            const controller = new AllergyController(allergyServiceInstance);

            await controller.updateAllergyDesignation(req as Request, res as Response);

            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(200)).toBe(true);
            expect(res.json.calledOnce).toBe(true);
            expect(res.json.calledWith({ allergy: updatedAllergy })).toBe(true);
        });

        it('should return a 404 error if the allergy is not found', async function () {
            const req: Partial<Request> = {
                params: { id: '1' },
                body: { designation: 'Updated Allergy Designation' },
            };
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const allergyServiceInstance = Container.get('AllergyService') as IAllergyService;
            const error = new NotFoundException('Allergy not found');
            sinon.stub(allergyServiceInstance, 'updateAllergyDesignation').rejects(error);

            const controller = new AllergyController(allergyServiceInstance);

            await controller.updateAllergyDesignation(req as Request, res as Response);

            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(404)).toBe(true);
            expect(res.json.calledOnce).toBe(true);
            expect(res.json.calledWith({ message: error.message })).toBe(true);
        });

        it('should return a 500 error for unexpected exceptions', async function () {
            const req: Partial<Request> = {
                params: { id: '1' },
                body: { designation: 'Updated Allergy Designation' },
            };
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const allergyServiceInstance = Container.get('AllergyService') as IAllergyService;
            const error = new Error('Unexpected error');
            sinon.stub(allergyServiceInstance, 'updateAllergyDesignation').rejects(error);

            const controller = new AllergyController(allergyServiceInstance);

            await controller.updateAllergyDesignation(req as Request, res as Response);

            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(500)).toBe(true);
            expect(res.json.calledOnce).toBe(true);
            expect(res.json.calledWith({ message: `Error updating allergy:${error}` })).toBe(true);
        });
    });

    /**
     * Tests for updateAllergyDescription
     */
    describe('updateAllergyDescription', () => {
        it('should successfully update an allergy description', async function () {
            const req: Partial<Request> = {
                params: { id: '1' },
                body: { description: 'Updated Allergy Description' },
            };
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const allergyServiceInstance = Container.get('AllergyService') as IAllergyService;
            const updatedAllergy = { id: '1', description: 'Updated Allergy Description' };
            sinon.stub(allergyServiceInstance, 'updateAllergyDescription').resolves(updatedAllergy);

            const controller = new AllergyController(allergyServiceInstance);

            await controller.updateAllergyDescription(req as Request, res as Response);

            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(200)).toBe(true);
            expect(res.json.calledOnce).toBe(true);
            expect(res.json.calledWith({ allergy: updatedAllergy })).toBe(true);
        });

        it('should return a 404 error if the allergy is not found', async function () {
            const req: Partial<Request> = {
                params: { id: '1' },
                body: { description: 'Updated Allergy Description' },
            };
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const allergyServiceInstance = Container.get('AllergyService') as IAllergyService;
            const error = new NotFoundException('Allergy not found');
            sinon.stub(allergyServiceInstance, 'updateAllergyDescription').rejects(error);

            const controller = new AllergyController(allergyServiceInstance);

            await controller.updateAllergyDescription(req as Request, res as Response);

            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(404)).toBe(true);
            expect(res.json.calledOnce).toBe(true);
            expect(res.json.calledWith({ message: error.message })).toBe(true);
        });

        it('should return a 500 error for unexpected exceptions', async function () {
            const req: Partial<Request> = {
                params: { id: '1' },
                body: { description: 'Updated Allergy Description' },
            };
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const allergyServiceInstance = Container.get('AllergyService') as IAllergyService;
            const error = new Error('Unexpected error');
            sinon.stub(allergyServiceInstance, 'updateAllergyDescription').rejects(error);

            const controller = new AllergyController(allergyServiceInstance);

            await controller.updateAllergyDescription(req as Request, res as Response);

            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(500)).toBe(true);
            expect(res.json.calledOnce).toBe(true);
            expect(res.json.calledWith({ message: `Error updating allergy:${error}` })).toBe(true);
        });
    });

    /**
     * Tests for updateAllergyEffects
     */
    describe('updateAllergyEffects', () => {
        it('should successfully update allergy effects', async function () {
            const req: Partial<Request> = {
                params: { id: '1' },
                body: { effects: ['Rashes', 'Swelling'] },
            };
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const allergyServiceInstance = Container.get('AllergyService') as IAllergyService;
            sinon.stub(allergyServiceInstance, 'updateAllergyEffects').resolves();

            const controller = new AllergyController(allergyServiceInstance);

            const updatedAllergy = await controller.updateAllergyEffects(req as Request, res as Response);

            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(200)).toBe(true);
            expect(res.json.calledOnce).toBe(true);
            expect(res.json.calledWith({ allergy: updatedAllergy })).toBe(true);
        });

        it('should return a 404 error if the allergy is not found', async function () {
            const req: Partial<Request> = {
                params: { id: '1' },
                body: { effects: ['Rashes', 'Swelling'] },
            };
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const allergyServiceInstance = Container.get('AllergyService') as IAllergyService;
            const error = new NotFoundException('Allergy not found');
            sinon.stub(allergyServiceInstance, 'updateAllergyEffects').rejects(error);

            const controller = new AllergyController(allergyServiceInstance);

            await controller.updateAllergyEffects(req as Request, res as Response);

            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(404)).toBe(true);
            expect(res.json.calledOnce).toBe(true);
            expect(res.json.calledWith({ message: error.message })).toBe(true);
        });

        it('should return a 500 error for unexpected exceptions', async function () {
            const req: Partial<Request> = {
                params: { id: '1' },
                body: { effects: ['Rashes', 'Swelling'] },
            };
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const allergyServiceInstance = Container.get('AllergyService') as IAllergyService;
            const error = new Error('Unexpected error');
            sinon.stub(allergyServiceInstance, 'updateAllergyEffects').rejects(error);

            const controller = new AllergyController(allergyServiceInstance);

            await controller.updateAllergyEffects(req as Request, res as Response);
            
            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(500)).toBe(true);
            expect(res.json.calledOnce).toBe(true);
            expect(res.json.calledWith({ message: `Error updating allergy:Error: Unexpected error` })).toBe(true);
        });
    });
});
