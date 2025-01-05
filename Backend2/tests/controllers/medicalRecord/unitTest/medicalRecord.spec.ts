import MedicalRecordController from "../../../../src/controllers/medicalRecordController";
import * as sinon from 'sinon';
import {NoMedicalRecordException} from "../../../../src/domain/MedicalRecord/NoMedicalRecordException";
import {
    NoMedicalRecordConditionsException
} from "../../../../src/domain/MedicalRecordCondition/NoMedicalRecordConditionsException";


describe("MedicalRecordController - MedicalRecordMedicalCondition - Unit", () => {
    const sandbox = sinon.createSandbox();
    let medicalRecordServiceMock: any;
    let medicalRecordController: MedicalRecordController;

    beforeEach(() => {
        medicalRecordServiceMock = {
            getMedicalRecordConditionByCode: sandbox.stub(),
            getMedicalRecordConditionByDesignation: sandbox.stub(),
            getMedicalRecordConditions: sandbox.stub(),
            getAllergies: sandbox.stub(),
            getFreeTexts: sandbox.stub(),
            addFreeText: sandbox.stub()
        };

        medicalRecordController = new MedicalRecordController(medicalRecordServiceMock);
    });

    afterEach(() => {
        sandbox.restore();
    });

    describe("getMedicalRecordConditionByCode", () => {
        it("should return medical record condition when service succeeds", async () => {
            const req = { params: { id: "test-id", code: "C10" } };
            const res = { status: sandbox.stub().returnsThis(), json: sandbox.stub() };
            const mockCondition = { id: "1", conditionDesignation: "Test Condition", doctorName: "John Doe" };

            medicalRecordServiceMock.getMedicalRecordConditionByCode.resolves(mockCondition);

            await medicalRecordController.getMedicalRecordConditionByCode(req, res);

            expect(res.status.calledWith(200)).toBe(true);
            expect(res.json.calledWith({ medicalRecordCondition: mockCondition })).toBe(true);
        });

        it("should return 900 when no medical record is found", async () => {
            const req = { params: { id: "test-id", code: "C10" } };
            const res = { status: sandbox.stub().returnsThis(), json: sandbox.stub() };

            medicalRecordServiceMock.getMedicalRecordConditionByCode.rejects(new NoMedicalRecordException());

            await medicalRecordController.getMedicalRecordConditionByCode(req, res);

            expect(res.status.calledWith(900)).toBe(true);
            expect(res.json.calledWith({ message: "No medical record found." })).toBe(true);
        });

        it("should return 500 on an unexpected error", async () => {
            const req = { params: { id: "test-id", code: "C10" } };
            const res = { status: sandbox.stub().returnsThis(), json: sandbox.stub() };

            medicalRecordServiceMock.getMedicalRecordConditionByCode.rejects(new Error("Expected error thrown during test"));

            await medicalRecordController.getMedicalRecordConditionByCode(req, res);

            expect(res.status.calledWith(500)).toBe(true);
            expect(res.json.calledWith({ message: "Error getting medical record condition." })).toBe(true);
        });
    });

    describe("getMedicalRecordConditionByDesignation", () => {
        it("should return medical record condition when service succeeds", async () => {
            const req = { params: { id: "test-id", designation: "Test Designation" } };
            const res = { status: sandbox.stub().returnsThis(), json: sandbox.stub() };
            const mockCondition = { id: "1", conditionDesignation: "Test Condition", doctorName: "John Doe" };

            medicalRecordServiceMock.getMedicalRecordConditionByDesignation.resolves(mockCondition);

            await medicalRecordController.getMedicalRecordConditionByDesignation(req, res);

            expect(res.status.calledWith(200)).toBe(true);
            expect(res.json.calledWith({ medicalRecordCondition: mockCondition })).toBe(true);
        });

        it("should return 900 when no medical record is found", async () => {
            const req = { params: { id: "test-id", designation: "Test Designation" } };
            const res = { status: sandbox.stub().returnsThis(), json: sandbox.stub() };

            medicalRecordServiceMock.getMedicalRecordConditionByDesignation.rejects(new NoMedicalRecordException());

            await medicalRecordController.getMedicalRecordConditionByDesignation(req, res);

            expect(res.status.calledWith(900)).toBe(true);
            expect(res.json.calledWith({ message: "No medical record found." })).toBe(true);
        });

        it("should return 500 on an unexpected error", async () => {
            const req = { params: { id: "test-id", designation: "Test Designation" } };
            const res = { status: sandbox.stub().returnsThis(), json: sandbox.stub() };

            medicalRecordServiceMock.getMedicalRecordConditionByDesignation.rejects(new Error("Expected error thrown during test"));

            await medicalRecordController.getMedicalRecordConditionByDesignation(req, res);

            expect(res.status.calledWith(500)).toBe(true);
            expect(res.json.calledWith({ message: "Error getting medical record conditions." })).toBe(true);
        });
    });

    describe("getMedicalRecordConditions", () => {
        it("should return medical record conditions when service succeeds", async () => {
            const req = { params: { id: "test-id" } };
            const res = { status: sandbox.stub().returnsThis(), json: sandbox.stub() };
            const mockConditions = [{ id: "1", doctorName: "John Doe", conditionDesignation: "Test Condition" }];

            medicalRecordServiceMock.getMedicalRecordConditions.resolves(mockConditions);

            await medicalRecordController.getMedicalRecordConditions(req, res);

            expect(res.status.calledWith(200)).toBe(true);
            expect(res.json.calledWith({ medicalRecordConditions: mockConditions })).toBe(true);
        });

        it("should return 850 when no medical record conditions are found", async () => {
            const req = { params: { id: "test-id" } };
            const res = { status: sandbox.stub().returnsThis(), json: sandbox.stub() };

            medicalRecordServiceMock.getMedicalRecordConditions.rejects(new NoMedicalRecordConditionsException());

            await medicalRecordController.getMedicalRecordConditions(req, res);
            
            expect(res.status.calledWith(850)).toBe(true);
            expect(res.json.calledWith({ message: "No conditions found for this medical record." })).toBe(true);
        });

        it("should return 500 on an unexpected error", async () => {
            const req = { params: { id: "test-id" } };
            const res = { status: sandbox.stub().returnsThis(), json: sandbox.stub() };

            medicalRecordServiceMock.getMedicalRecordConditions.rejects(new Error("Expected error thrown during test"));

            await medicalRecordController.getMedicalRecordConditions(req, res);

            expect(res.status.calledWith(500)).toBe(true);
            expect(res.json.calledWith({ message: "Error getting medical record conditions." })).toBe(true);
        });
    });

    describe("getAllergies",  () => {
        it("should return medical record allergies when service succeeds", async () => {
            const req = { params: { id: "test-id" } };
            const res = { status: sandbox.stub().returnsThis(), json: sandbox.stub() };
            const mockAllergies = [{ allergy: "Allergy-Test", medicalRecordId: "Test-Id" ,doctor: "John Doe", comment: "Comment-Test" }];

            medicalRecordServiceMock.getAllergies.resolves(mockAllergies);

            await medicalRecordController.getAllergies(req, res);

            expect(res.status.calledWith(200)).toBe(true);
            expect(res.json.calledWith({ body: mockAllergies })).toBe(true);
        });

        it("should return NoMedicalRecordException", async () => {
            const req = { params: { id: "test-id" } };
            const res = { status: sandbox.stub().returnsThis(), json: sandbox.stub() };

            medicalRecordServiceMock.getAllergies.rejects(new NoMedicalRecordException());

            await medicalRecordController.getAllergies(req, res);

            expect(res.status.calledWith(500)).toBe(true);
            expect(res.json.calledWith({ message: "No medical record found." })).toBe(true);
        });
        
        it("should return empty MedicalRecordAllergy list", async () => {
            const req = { params: { id: "test-id" } };
            const res = { status: sandbox.stub().returnsThis(), json: sandbox.stub() };

            medicalRecordServiceMock.getAllergies.resolves([]);

            await medicalRecordController.getAllergies(req, res);

            expect(res.status.calledWith(200)).toBe(true);
            expect(res.json.calledWith({body: []})).toBe(true);
        });
    });
    describe('addFreeText', function () {
        it('should create a free text comment successfully using service stub', async function () {

            const body = {
                medicalRecordId: '20250100005',
                doctorId: 'N202400005',
                comment: 'only has one teeth'
            };

            const req = { body };

            const res = {
                status: sinon.stub().returnsThis(),
                json: sinon.stub()
            };

            const next = sinon.spy();

            const medicalRecordServiceMock = {
                addFreeText: sinon.stub().resolves({ message: 'Comment created successfully' })
            };

            const controller = async (req, res, next) => {
                try {
                    const result = await medicalRecordServiceMock.addFreeText(req.body);
                    res.status(200).json(result);
                } catch (error) {
                    next(error);
                }
            };


            await controller(req, res, next);

            expect(medicalRecordServiceMock.addFreeText.calledOnce).toBe(true);
            expect(medicalRecordServiceMock.addFreeText.calledWith(req.body)).toBe(true);
            expect(res.status.calledOnce).toBe(true);
            expect(res.status.calledWith(200)).toBe(true);
            expect(res.json.calledOnce).toBe(true);
            expect(res.json.calledWith({ message: 'Comment created successfully' })).toBe(true);
            expect(next.called).toBe(false);
        });
    });





    describe('getFreeTexts', ()=>{
        it("should return medical record free texts when service succeeds", async()=>{
            const req = { params: { id: "test-id" } };
            const res = { status: sandbox.stub().returnsThis(), json: sandbox.stub() };
            const mockFreeTexts = [{ medicalRecord: "20250100005", doctorId: "N202400005" ,comment: "He needs a lung transplant" }];

            medicalRecordServiceMock.getFreeTexts.resolves(mockFreeTexts);

            await medicalRecordController.getFreeTexts(req, res);
            expect(res.status.calledWith(200)).toBe(true);
            expect(res.json.calledWith({ body: mockFreeTexts })).toBe(true);
        })

        it("should return NoMedicalRecordException", async () => {
            const req = { params: { id: "test-id" } };
            const res = { status: sandbox.stub().returnsThis(), json: sandbox.stub() };

            medicalRecordServiceMock.getFreeTexts.rejects(new NoMedicalRecordException());

            await medicalRecordController.getFreeTexts(req, res);

            expect(res.status.calledWith(500)).toBe(true);
            expect(res.json.calledWith({ message: "No medical record found." })).toBe(true);
        });

        it("should return empty MedicalRecordFreeText list", async () => {
            const req = { params: { id: "test-id" } };
            const res = { status: sandbox.stub().returnsThis(), json: sandbox.stub() };

            medicalRecordServiceMock.getFreeTexts.resolves([]);

            await medicalRecordController.getFreeTexts(req, res);

            expect(res.status.calledWith(200)).toBe(true);
            expect(res.json.calledWith({body: []})).toBe(true);
        });

    });



});

