import 'reflect-metadata';
import * as sinon from 'sinon';
import { Response, Request, NextFunction } from 'express';
import { Container } from 'typedi';
import IMedicalConditionService from '../../../../src/services/IServices/IMedicalConditionService';
import IMedicalConditionDTO from '../../../../src/dto/IMedicalConditionDTO';
import MedicalConditionController from "../../../../src/controllers/MedicalConditionController";
import {AppError} from "../../../../src/domain/Shared/Exceptions/AppError";
import {NotFoundException} from "../../../../src/Exceptions/NotFoundException";

describe('MedicalConditionController - Unit Test', function () {
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

    it('should create a medical condition successfully using service stub', async function () {

        const body: IMedicalConditionDTO = {
            code: 'C123',
            designation: 'Hypertension',
            description: 'High blood pressure',
            symptomsList: ['Headache', 'Blurred vision'],
        };
        const req: Partial<Request> = { body };
        const res = {
            status: sinon.stub().returnsThis(),
            json: sinon.spy(),
        };
        const next: Partial<NextFunction> = () => {};

        const medicalConditionServiceInstance = Container.get('MedicalConditionService') as IMedicalConditionService;
        sinon.stub(medicalConditionServiceInstance, 'createMedicalCondition').resolves();

        const controller = new MedicalConditionController(medicalConditionServiceInstance);

        await controller.createMedicalCondition(req as Request, res as Response);
        
        expect(res.status.calledOnce).toBe(true);
        expect(res.status.calledWith(200)).toBe(true);
        expect(res.json.calledOnce).toBe(true);
        
    });

    it('should return an error if service throws AppError', async function () {

        const body: IMedicalConditionDTO = {
            code: 'C123',
            designation: 'Hypertension',
            description: 'High blood pressure',
            symptomsList: ['Headache', 'Blurred vision'],
        };
        const req: Partial<Request> = { body };
        const res= {
            status: sinon.stub().returnsThis(),
            json: sinon.spy(),
        };
        const next: Partial<NextFunction> = () => {};

        const medicalConditionServiceInstance = Container.get('MedicalConditionService') as IMedicalConditionService;
        const error = new AppError("CODE_NULL_UNDEFINED");
        sinon.stub(medicalConditionServiceInstance, 'createMedicalCondition').rejects(error);

        const controller = new MedicalConditionController(medicalConditionServiceInstance);

        await controller.createMedicalCondition(req as Request, res as Response);
        
        expect(res.status.calledOnce).toBe(true);
        expect(res.status.calledWith(error.code)).toBe(true);
        expect(res.json.calledOnce).toBe(true);
        expect(res.json.calledWith({ message: error.message })).toBe(true);
        
    });

    it('should return a 500 error for unexpected exceptions', async function () {

        const body: IMedicalConditionDTO = {
            code: 'C123',
            designation: 'Hypertension',
            description: 'High blood pressure',
            symptomsList: ['Headache', 'Blurred vision'],
        };
        const req: Partial<Request> = { body };
        const res = {
            status: sinon.stub().returnsThis(),
            json: sinon.spy(),
        };
        const next: Partial<NextFunction> = () => {};

        const medicalConditionServiceInstance = Container.get('MedicalConditionService') as IMedicalConditionService;
        const error = new Error('Unexpected error');
        sinon.stub(medicalConditionServiceInstance, 'createMedicalCondition').rejects(error);

        const controller = new MedicalConditionController(medicalConditionServiceInstance);

        await controller.createMedicalCondition(req as Request, res as Response);
        
        expect(res.status.calledOnce).toBe(true);
        expect(res.status.calledWith(500)).toBe(true);
        expect(res.json.calledOnce).toBe(true);
        expect(res.json.calledWith({ message: 'Code/Designation already exists' })).toBe(true);
        
    });
    
    describe('updateMedicalConditionDescription', function () {
        it('should update the description of a medical condition successfully using service stub', async function () {
                const req: Partial<Request> = { params: { id: '1' }, body: { description: 'New description' } };
                const res = {
                    status: sinon.stub().returnsThis(),
                    json: sinon.spy(),
                };
    
                const medicalConditionServiceInstance = Container.get('MedicalConditionService') as IMedicalConditionService;
                const updatedMedicalCondition = { id: '1', description: 'New description' };
                sinon.stub(medicalConditionServiceInstance, 'updateMedicalConditionDescription').resolves(updatedMedicalCondition);
    
                const controller = new MedicalConditionController(medicalConditionServiceInstance);
    
                await controller.updateMedicalConditionDescription(req as Request, res as Response);
                
                expect(res.status.calledOnce).toBe(true);
                expect(res.status.calledWith(200)).toBe(true);
                expect(res.json.calledOnce).toBe(true);
                expect(res.json.calledWith({ message: 'Medical Condition description updated successfully' })).toBe(true);
        });
        
        it('should return 404 if medical condition is not found', async function () {
            const req: Partial<Request> = { params: { id: '1' }, body: { description: 'New description' } };
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const medicalConditionServiceInstance = Container.get('MedicalConditionService') as IMedicalConditionService;
            const error = new NotFoundException('Medical Condition not found');
            sinon.stub(medicalConditionServiceInstance, 'updateMedicalConditionDescription').rejects(error);

            const controller = new MedicalConditionController(medicalConditionServiceInstance);

            await controller.updateMedicalConditionDescription(req as Request, res as Response);
            
            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(404)).toBe(true);
            expect(res.json.calledOnce).toBe(true);
            expect(res.json.calledWith({ message: error.message })).toBe(true);
        });
        
        it('should fail if description is empty', async function () {
            const req: Partial<Request> = { params: { id: '1' }, body: { description: '' } };
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const medicalConditionServiceInstance = Container.get('MedicalConditionService') as IMedicalConditionService;
            const error = new AppError("DESCRIPTION_INVALID_WHITESPACE");
            sinon.stub(medicalConditionServiceInstance, 'updateMedicalConditionDescription').rejects(error);
            
            const controller = new MedicalConditionController(medicalConditionServiceInstance);

            await controller.updateMedicalConditionDescription(req as Request, res as Response);
            
            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(500)).toBe(true);
            expect(res.json.calledOnce).toBe(true);
            expect(res.json.calledWith({ message: 'Description can not be empty.' })).toBe(true);
        });

        it('should fail if description is null', async function () {
            const req: Partial<Request> = { params: { id: '1' }, body: { description: null } };
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const medicalConditionServiceInstance = Container.get('MedicalConditionService') as IMedicalConditionService;
            const error = new AppError("DESCRIPTION_NULL_UNDEFINED");
            sinon.stub(medicalConditionServiceInstance, 'updateMedicalConditionDescription').rejects(error);

            const controller = new MedicalConditionController(medicalConditionServiceInstance);

            await controller.updateMedicalConditionDescription(req as Request, res as Response);

            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(500)).toBe(true);
            expect(res.json.calledOnce).toBe(true);
            expect(res.json.calledWith({ message: 'Description can not be null or undefined.' })).toBe(true);
        });

        it('should fail if description is invalid', async function () {
            const req: Partial<Request> = { params: { id: '1' }, body: {
                description: 'Allergy to natural rubber latex with associated hypersensitivity reactions and cross-reactivity to certain tropical fruits, including but not limited to banana, avocado, kiwi, chestnut, and passion fruit, which can manifest in diverse symptoms ranging from mild cutaneous reactions, such as localized itching, redness, and swelling, to more severe systemic responses, including respiratory distress, gastrointestinal discomfort, and anaphylaxis. This condition is often associated with a type I hypersensitivity reaction mediated by immunoglobulin E (IgE) antibodies that recognize specific proteins present in latex derived from the sap of the Hevea brasiliensis tree. The prevalence of latex-fruit syndrome, a subset of this allergy, is attributed to the structural similarity between latex proteins and proteins found in certain fruits, leading to an immune response when these allergens are encountered. Common triggers include exposure to latex products such as gloves, balloons, and medical equipment, which can cause direct contact reactions or airborne sensitization. Diagnosis typically involves a combination of clinical history, skin prick testing, and serological tests for specific IgE antibodies, while management focuses on strict avoidance of known allergens, the use of barrier protection methods, and the readiness to administer emergency treatments like epinephrine in cases of anaphylaxis. Education on the hidden sources of latex and cross-reactive fruits, alongside clear communication with healthcare providers and caretakers, is crucial for individuals living with this condition to mitigate risks and maintain quality of life. Research into alternative hypoallergenic materials and immunotherapy options continues to provide hope for improved management and potential desensitization protocols in the future.',
            } };
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const medicalConditionServiceInstance = Container.get('MedicalConditionService') as IMedicalConditionService;
            const error = new AppError("DESCRIPTION_INVALID_LENGTH");
            sinon.stub(medicalConditionServiceInstance, 'updateMedicalConditionDescription').rejects(error);

            const controller = new MedicalConditionController(medicalConditionServiceInstance);

            await controller.updateMedicalConditionDescription(req as Request, res as Response);

            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(500)).toBe(true);
            expect(res.json.calledOnce).toBe(true);
            expect(res.json.calledWith({ message: 'Description must have a maximum of 2048 characters.' })).toBe(true);
        });
    });

    describe('updateMedicalConditionSymptoms', function () {
        it('should update the symptoms of a medical condition successfully', async function () {
            const req: Partial<Request> = { params: { id: '1' }, body: { symptomsList: ['Headache', 'Blurred vision'] } };
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const medicalConditionServiceInstance = Container.get('MedicalConditionService') as IMedicalConditionService;
            sinon.stub(medicalConditionServiceInstance, 'updateMedicalConditionSymptoms').resolves();

            const controller = new MedicalConditionController(medicalConditionServiceInstance);

            await controller.updateMedicalConditionSymptoms(req as Request, res as Response);
            
            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(200)).toBe(true);
            expect(res.json.calledOnce).toBe(true);
            expect(res.json.calledWith({ message: 'Medical Condition symptoms updated successfully' })).toBe(true);
        });
        
        it('should return 404 if medical condition is not found', async function () {
            const req: Partial<Request> = { params: { id: '1' }, body: { symptomsList: ['Headache', 'Blurred vision'] } };
            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.spy(),
            };

            const medicalConditionServiceInstance = Container.get('MedicalConditionService') as IMedicalConditionService;
            const error = new NotFoundException('Medical Condition not found');
            sinon.stub(medicalConditionServiceInstance, 'updateMedicalConditionSymptoms').rejects(error);

            const controller = new MedicalConditionController(medicalConditionServiceInstance);

            await controller.updateMedicalConditionSymptoms(req as Request, res as Response);
            
            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(404)).toBe(true);
            expect(res.json.calledOnce).toBe(true);
            expect(res.json.calledWith({ message: error.message })).toBe(true);
        });
    });
});
