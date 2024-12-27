import {NextFunction, Request, Response} from "express";


export default interface IMedicalRecordController{
    createMedicalRecord(req: Request, res: Response, next: NextFunction);
    getAllergies(req: Request, res: Response, next: NextFunction);
    getMedicalRecordConditions(req: Request, res: Response, next: NextFunction);
    getMedicalRecordConditionByCode(req: Request, res: Response, next: NextFunction);
    getMedicalRecordConditionByDesignation(req: Request, res: Response, next: NextFunction);

    updateMedicalRecordConditions(req, res, next);

    getAllMedicalRecordConditions(req, res, next);

    createFamilyHistory(req, res, next);
}
