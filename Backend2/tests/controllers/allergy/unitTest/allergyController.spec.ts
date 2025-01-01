import 'reflect-metadata';
import * as sinon from 'sinon';
import { Request, Response, NextFunction } from 'express';
import { Container } from 'typedi';
import IAllergyService from '../../../../src/services/IServices/IAllergyService';
import { NotFoundException } from '../../../../src/Exceptions/NotFoundException';
import { AppError } from '../../../../src/domain/Shared/Exceptions/AppError';
import AllergyController from "../../../../src/controllers/allergyController";

describe('AllergyController - Unit Test', function () {
    const sandbox = sinon.createSandbox();

    beforeEach(function () {
        Container.reset();

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
