import {Request,Response,NextFunction} from "express";

export default interface IMedicalConditionController {
    createMedicalCondition(req: Request, res: Response, next: NextFunction);
}
