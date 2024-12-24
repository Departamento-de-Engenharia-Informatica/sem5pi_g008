import {NextFunction, Request, Response} from "express";


export default interface IMedicalRecordController{
    createMedicalRecord(req: Request, res: Response, next: NextFunction);
    getAllergies(req: Request, res: Response, next: NextFunction);
    updateMedicalRecordConditions(req, res, next);
}
