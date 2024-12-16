import {Mapper} from "../core/infra/Mapper";
import {Document, Model} from "mongoose";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {MedicalRecordCondition} from "../domain/MedicalRecordCondition/MedicalRecordCondition";
import IMedicalRecordConditionDTO from "../dto/IMedicalRecordConditionDTO";
import {MedicalConditionMap} from "./MedicalConditionMap";
import {IMedicalRecordConditionPersistence} from "../dataschema/IMedicalRecordConditionPersistence";

export class MedicalRecordConditionMapper extends Mapper<MedicalRecordCondition> {

  public static toDTO(domain: MedicalRecordCondition): IMedicalRecordConditionDTO {
    return {
      condition: MedicalConditionMap.toDTO(domain.condition),
      doctorId: domain.doctorId,
      comment: domain.comment
    } as IMedicalRecordConditionDTO;
  }

  public static toDomain (medicalRecordCondition: any | Model<IMedicalRecordConditionPersistence & Document> ): MedicalRecordCondition {

    const medicalRecordConditionResult = MedicalRecordCondition.create(
      medicalRecordCondition,
      new UniqueEntityID(medicalRecordCondition.domainId)
    );

    medicalRecordConditionResult.isFailure ? console.log(medicalRecordConditionResult.error) : '';

    return medicalRecordConditionResult.isSuccess ? medicalRecordConditionResult.getValue() : null;
  }

  public static toPersistence (medicalRecordCondition: MedicalRecordCondition, id?: Number): any {
    if(id === undefined) {
      return {
        domainId: medicalRecordCondition.domainId,
        condition: medicalRecordCondition.condition,
        comment: medicalRecordCondition.comment,
        doctorId: medicalRecordCondition.doctorId,
      }
    }

    return {
      domainId: id,
      condition: medicalRecordCondition.condition,
      comment: medicalRecordCondition.comment,
      doctorId: medicalRecordCondition.doctorId,
    }
  }

}
