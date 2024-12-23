import { Repo } from "../../core/infra/Repo";
import {MedicalCondition} from "../../domain/MedicalCondition/MedicalCondition";
import {Code} from "../../domain/MedicalCondition/code";
import {Designation} from "../../domain/MedicalCondition/designation";

export default interface IMedicalConditionRepo extends Repo<MedicalCondition> {
    save(medicalCondition: MedicalCondition, id?: number): Promise<MedicalCondition>;
    getMedicalConditionByBusinessId(medicalConditionId: string): Promise<any>;
    getAll(): Promise<MedicalCondition[]>;
    getMedicalConditionByCode(code: Code): Promise<any>;
    getMedicalConditionByDesignation(designation: Designation): Promise<any>;

}
