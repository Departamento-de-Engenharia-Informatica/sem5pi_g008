import {NextFunction, Request, Response} from "express";


export default interface IMedicalRecordController{
    createMedicalRecord(req: Request, res: Response, next: NextFunction);
    getAllergies(req: Request, res: Response, next: NextFunction);
    getFreeTexts(req: Request, res: Response, next: NextFunction);
    addFreeText(req: Request, res: Response, next: NextFunction);

    getMedicalRecordConditions(req: Request, res: Response, next: NextFunction);
    getMedicalRecordConditionByCode(req: Request, res: Response, next: NextFunction);
    getMedicalRecordConditionByDesignation(req: Request, res: Response, next: NextFunction);


    createFamilyHistory(req: Request, res: Response, next: NextFunction);

    getAllMedicalRecordConditions(req, res, next);

    getMedicalRecordFamilyHistoryWithIds(req, res, next);

    updateMedicalRecordConditionComment(req, res, next);
    updateMedicalRecordAllergiesComment(req, res, next);
}
