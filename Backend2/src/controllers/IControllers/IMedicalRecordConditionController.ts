import {NextFunction, Request, Response} from "express";

export default interface IMedicalRecordConditionController {
    getMedicalRecordConditions(req: Request, res: Response, next: NextFunction);
}