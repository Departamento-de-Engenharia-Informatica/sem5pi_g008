import {Request,Response,NextFunction} from "express";

export default interface IMedicalConditionController {
    createMedicalCondition(req: Request, res: Response, next: NextFunction);
    searchMedicalConditionsCode(req: Request, res: Response, next: NextFunction);
    searchMedicalConditionsDesignation(req: Request, res: Response, next: NextFunction);

    getAllMedicalConditions(req: Request, res: Response, next: NextFunction);
}
