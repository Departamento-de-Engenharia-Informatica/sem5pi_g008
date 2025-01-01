import { Repo } from "../../core/infra/Repo";
import {MedicalCondition} from "../../domain/MedicalCondition/MedicalCondition";
import {Code} from "../../domain/Shared/code";
import {Designation} from "../../domain/Shared/designation";

export default interface IMedicalConditionRepo extends Repo<MedicalCondition> {
    save(medicalCondition: MedicalCondition, id?: number): Promise<MedicalCondition>;
    getMedicalConditionByBusinessId(medicalConditionId: string): Promise<any>;
    getByDomainId(id: number): Promise<MedicalCondition>;
    update(medicalCondition: MedicalCondition): Promise<MedicalCondition>;
    updateUsingDomainId(medicalCondition: MedicalCondition, ...fieldsToUpdate: string[]): Promise<MedicalCondition>;
    getAll(): Promise<MedicalCondition[]>;
    getMedicalConditionByCode(code: Code): Promise<any>;
    getMedicalConditionByDesignation(designation: Designation): Promise<any>;

}
