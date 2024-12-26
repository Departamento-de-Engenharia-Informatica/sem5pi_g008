import {Mapper} from "../core/infra/Mapper";
import {MedicalCondition} from "../domain/MedicalCondition/MedicalCondition";
import IMedicalConditionDTO from "../dto/IMedicalConditionDTO";
import {Document, Model} from "mongoose";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {IMedicalConditionPersistence} from "../dataschema/IMedicalConditionPersistence";
import {Code} from "../domain/MedicalCondition/code";
import {Designation} from "../domain/MedicalCondition/designation";
import {Description} from "../domain/MedicalCondition/description";

export class MedicalConditionMap extends Mapper<MedicalCondition> {

    public static toDTO(medicalCondition: MedicalCondition): IMedicalConditionDTO {

        return {
            domainId: medicalCondition.domainId.id.toValue(),
            code: medicalCondition.code.value,
            designation: medicalCondition.designation.value,
            description: medicalCondition.description.value,
            symptomsList: medicalCondition.symptomsList
        } as IMedicalConditionDTO;
    }

    public static toDomain (medicalCondition: any | Model<IMedicalConditionPersistence & Document> ): MedicalCondition {

        const medicalConditionProps = {
            _id: medicalCondition._id.toString(),
            code: Code.create(medicalCondition.code).getValue(),
            designation: Designation.create(medicalCondition.designation).getValue(),
            description: Description.create(medicalCondition.description).getValue(),
            symptomsList: medicalCondition.symptomsList
        }

        const medicalConditionOrError = MedicalCondition.create(
            medicalConditionProps,
            new UniqueEntityID(medicalCondition.domainId)
        );

        medicalConditionOrError.isFailure ? console.log(medicalConditionOrError.error) : '';

        return medicalConditionOrError.isSuccess ? medicalConditionOrError.getValue() : null;
    }

    public static toPersistence (medicalCondition: MedicalCondition, id: Number): IMedicalConditionPersistence {

        if(id === undefined) {
            return {
                domainId: <number>medicalCondition.domainId.id.toValue(),
                code: medicalCondition.code.value,
                designation: medicalCondition.designation.value,
                description: medicalCondition.description.value,
                symptomsList: medicalCondition.symptomsList
            }
        }

        return {
            domainId: <number>id,
            code: medicalCondition.code.value,
            designation: medicalCondition.designation.value,
            description: medicalCondition.description.value,
            symptomsList: medicalCondition.symptomsList
        }
    }

}
