import {NextFunction, Request, Response} from "express";


export default interface IMedicalRecordController{
    updateMedicalRecord(req: Request, res: Response, next: NextFunction);
}