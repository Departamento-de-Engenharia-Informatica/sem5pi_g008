import { Result } from "../../core/logic/Result";
import IMedicalConditionDTO from "../../dto/IMedicalConditionDTO";

export default interface IMedicalConditionService  {
    createMedicalCondition(medicalCondition: IMedicalConditionDTO): Promise<any>;
    getAllMedicalConditions(): Promise<IMedicalConditionDTO[]>;
}
