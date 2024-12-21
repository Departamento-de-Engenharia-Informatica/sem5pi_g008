import {Repo} from "../../core/infra/Repo";
import {MedicalRecordCondition} from "../../domain/MedicalRecordCondition/MedicalRecordCondition";

export default interface IMedicalRecordConditionRepo extends Repo<MedicalRecordCondition> {
  save(medicalCondition: MedicalRecordCondition): Promise<MedicalRecordCondition>;
}
